#include "jsonstorage.h"
#include <iostream>
#include <fstream>
#include <memory>
#include <cmath>

JsonStorage::JsonStorage(std::ostream& ostream, int precision) : m_oStream(ostream)
{
    m_oStream.precision(precision);
}

bool JsonStorage::start(JsonStorage::GroupType type)
{
    if (m_groupNameStack.size()) {
        std::cerr << "JsonStorage::start failed: Already started!\n";
        return false;
    }
    m_topGroupType = type;
    if (m_oStream.bad()) {
        std::cerr << "JsonStorage::start failed: Bad out stream!\n";
        return false;
    }
    if (m_topGroupType == DICT) m_oStream << "{\n";
    else m_oStream << "[\n";
    m_firstElement = true;

    // use fixed form for floats
    m_oStream << std::fixed;

    return true;
}

bool JsonStorage::end()
{
    if (m_oStream.bad()) {
        std::cerr << "JsonStorage::end failed: Bad out stream!\n";
        return false;
    }
    bool rc = true;
    while(m_groupNameStack.size()) {
        rc &= endGroup();
    }
    if (m_topGroupType == DICT) m_oStream << "\n}\n";
    else m_oStream << "\n]\n";
    return true;
}

bool JsonStorage::startGroup(GroupType type, const std::string &name)
{
    if (!handleFirstElementInGroup()) {
        std::cerr << "startGroup failed: Failed first element in group handling" << std::endl;
        return false;
    }
    auto groupType = m_topGroupType;
    if (!m_groupTypeStack.empty()) groupType = m_groupTypeStack.top();
    switch(groupType) {
    case DICT:
        m_oStream << "\"" << name  << "\": "; break;
    case LIST:
        break;
    }
    switch(type) {
    case DICT:
        m_oStream << "{\n"; break;
    case LIST:
        m_oStream << "[\n"; break;
    }
    m_groupNameStack.push(name);
    m_groupTypeStack.push(type);
    m_firstElementMap[name] = true;
    m_firstElement = true;
    return true;
}


bool JsonStorage::endGroup()
{
    if (m_groupNameStack.empty()) {
        std::cerr << "endGroup faled: Stack is empty" << std::endl;
        return false;
    }
    auto name = m_groupNameStack.top();
    auto type = m_groupTypeStack.top();
    m_groupNameStack.pop();
    m_groupTypeStack.pop();
    m_firstElementMap.erase(name);
    if (m_oStream.bad()) {
        std::cerr << "endGroup faled: Bad out stream" << std::endl;
        return false;
    }
    m_oStream << "\n";
    for (int i=0;i<m_groupNameStack.size();++i) {
        m_oStream << "  ";
    }
    switch(type) {
    case DICT:
        m_oStream << "}"; break;
    case LIST:
        m_oStream << "]"; break;
    }
    m_firstElement = false;
    return true;
}

bool JsonStorage::addVariable(const std::string &name, const std::string &value)
{
    if (!handleFirstElementInGroup()) {
        std::cerr << "addVariable faled: Failed first element in group handling" << std::endl;
        return false;
    }
    auto type = m_topGroupType;
    if (!m_groupTypeStack.empty()) type = m_groupTypeStack.top();
    switch(type) {
    case DICT:
        m_oStream << "\"" << name << "\"" << ": " << "\"" << value << "\""; break;
    case LIST:
        m_oStream <<  "\"" << value << "\""; break;
    }
    m_firstElement = false;
    return true;
}

bool JsonStorage::addVariable(const std::string &name, double value)
{
    if (std::isnan(value)) {
        std::cerr << "addVariable \'" << name << "\' failed: Value is NaN. Will not add" << std::endl;
        return false;
    }
    if (!handleFirstElementInGroup()) {
        std::cerr << "addVariable failed: Failed first element in group handling" << std::endl;
        return false;
    }

    auto type = m_topGroupType;
    if (!m_groupTypeStack.empty()) type = m_groupTypeStack.top();
    switch(type) {
    case DICT:
        m_oStream << "\"" << name << "\"" << ": " << std::scientific <<  value; break;
    case LIST:
        m_oStream << std::scientific << value; break;
    }
    m_firstElement = false;
    return true;
}

bool JsonStorage::addVariable(const std::string &name, int value)
{
    if (!handleFirstElementInGroup()) {
        std::cerr << "addVariable faled: Failed first element in group handling" << std::endl;
        return false;
    }

    auto type = m_topGroupType;
    if (!m_groupTypeStack.empty()) type = m_groupTypeStack.top();
    switch(type) {
    case DICT:
        m_oStream << "\"" << name << "\"" << ": " << value; break;
    case LIST:
        m_oStream << value; break;
    }
    m_firstElement = false;
    return true;
}

bool JsonStorage::addVariable(const std::string& name, bool value)
{
    if (!handleFirstElementInGroup()) {
        std::cerr << "addVariable faled: Failed first element in group handling" << std::endl;
        return false;
    }

    auto type = m_topGroupType;
    if (!m_groupTypeStack.empty()) type = m_groupTypeStack.top();
    switch(type) {
    case DICT:
        m_oStream << "\"" << name << "\"" << ": " << (value ? "true" : "false"); break;
    case LIST:
        m_oStream << (value ? "true" : "false"); break;
    }
    m_firstElement = false;
    return true;
}

bool JsonStorage::addVariable(const std::string &name, const double *values, size_t size)
{
    if (!handleFirstElementInGroup()) {
        std::cerr << "addVariable faled: Failed first element in group handling" << std::endl;
        return false;
    }

    auto type = m_topGroupType;
    if (!m_groupTypeStack.empty()) type = m_groupTypeStack.top();
    switch(type) {
    case DICT:
    {
        if (!size) m_oStream << "\"" << name << "\"" << ": [] ";
        else {
            if (std::isnan(values[0])) {
                m_oStream << "\"" << name << "\"" << ": [NaN";
            } else {
                m_oStream << "\"" << name << "\"" << ": [" << values[0];
            }
            for (size_t i=1;i<size;++i) {
                if (std::isnan(values[i])) {
                    m_oStream << ", NaN";
                } else {
                    m_oStream << ", " << values[i];
                }
            }
            m_oStream << "] ";
        }
    } break;
    case LIST:
    {
        if (!size) m_oStream << " [] ";
        else {
            if (std::isnan(values[0])) {
                 m_oStream << "[NaN";
            } else {
                m_oStream << "[" << values[0];
            }
            for (size_t i=1;i<size;++i) {
                 if (std::isnan(values[i])) {
                 } else {
                    m_oStream << ", " << values[i];
                 }
            }
            m_oStream << "] ";
        }
    }
    }
    m_firstElement = false;
    return true;
}

bool JsonStorage::addVariable(const std::string &name, const int *values, size_t size)
{
    if (!handleFirstElementInGroup()) {
        std::cerr << "addVariable faled: Failed first element in group handling" << std::endl;
        return false;
    }

    auto type = m_topGroupType;
    if (!m_groupTypeStack.empty()) type = m_groupTypeStack.top();
    switch(type) {
    case DICT:
    {
        if (!size) m_oStream << "\"" << name << "\"" << ": [] ";
        else {
            m_oStream << "\"" << name << "\"" << ": [" << values[0];
            for (size_t i=1;i<size;++i) {
                m_oStream << ", " << values[i];
            }
            m_oStream << "] ";
        }
    } break;
    case LIST:
    {
        if (!size) m_oStream << " [] ";
        else {
            m_oStream << "[" << values[0];
            for (size_t i=1;i<size;++i) {
                m_oStream << ", " << values[i];
            }
            m_oStream << "] ";
        }
    }
    }
    m_firstElement = false;
    return true;
}


bool JsonStorage::handleFirstElementInGroup()
{
    if (m_oStream.bad()) {
        std::cerr << "handleFirstElementInGroup failed: Bad out stream" << std::endl;
        return false;
    }
    bool firstelement = m_firstElement;
    std::string parent;
    if (!firstelement) {
        m_oStream << ",\n";
    }

	for (int i=0;i<m_groupNameStack.size();++i) {
		m_oStream << "  ";
	}

    return true;
}


namespace {

std::unique_ptr<JsonStorage> currentStorageHandler = nullptr;
std::fstream outstream;

std::string stdString(const char* inname, FORTRAN_LEN len)
{
    int i;
    for (i = len-1;i>=0;--i) {
        if (inname[i] != ' ' && inname[i] != '\000') break;
    }
    return std::string(inname, static_cast<size_t>(i+1));
}

}

void json_start_list_(const char* filename, int* precision, int* rc, FORTRAN_LEN _filename_sz)
{
    if (rc) *rc = 0;
    if (currentStorageHandler) {
        std::cerr << "In json_start_list_(" << filename << "): Event storage already started!\n";
        if (rc) *rc = -1;
        return;
    }
    auto fnam = stdString(filename, _filename_sz);
    std::cout << "Open " << fnam << ", " << _filename_sz << " for writing" << std::endl;
    outstream.open(stdString(filename, _filename_sz), std::ios_base::out);
    if (outstream.bad())  {
        std::cerr << "In json_start_list_(" << filename << "): Could not open out stream!\n";
        if (rc) *rc = -2;
    }
    currentStorageHandler = std::unique_ptr<JsonStorage>(new JsonStorage(outstream, *precision));
    if (!currentStorageHandler->start(JsonStorage::LIST)) {
        std::cerr << "In json_start_list_(" << filename << "): Start failed!\n";
        if (rc) *rc = -3;
    }
}

void json_start_dict_(const char* filename, int* precision, int* rc, FORTRAN_LEN _filename_sz)
{
    if (rc) *rc = 0;
    if (currentStorageHandler) {
        std::cerr << "In json_start_dict_(" << filename << "): Event storage already started!\n";
        if (rc) *rc = -1;
        return;
    }
    outstream.open(stdString(filename, _filename_sz), std::ios_base::out);
    if (outstream.bad())  {
        std::cerr << "In json_start_list_(" << filename << "): Could not open out stream!\n";
        if (rc) *rc = -2;
    }
    currentStorageHandler = std::unique_ptr<JsonStorage>(new JsonStorage(outstream, *precision));
    if (!currentStorageHandler->start(JsonStorage::DICT)) {
        std::cerr << "In json_start_dict_(" << filename << "): Start failed!\n";
        if (rc) *rc = -2;
    }
}

void json_end_(int* rc)
{
    if (rc) *rc = 0;
    if (!currentStorageHandler) {
        std::cerr << "In json_end_: No active event storage!\n";
        if (rc) *rc = -1;
        return;
    }
    if (!currentStorageHandler->end()) {
        std::cerr << "In json_end_: Failed to end event storage\n";
        return;
    }
    currentStorageHandler = nullptr;
    outstream.close();
}

void json_start_dict_group_(const char* name, int* rc, FORTRAN_LEN _name_sz)
{
    if (rc) *rc = 0;
    if (!currentStorageHandler) {
        std::cerr << "In json_start_dict_group_: No active event storage!\n";
        if (rc) *rc = -1;
        return;
    }
    if (!currentStorageHandler->startGroup(JsonStorage::DICT, stdString(name, _name_sz))) {
        std::cerr << "In json_start_dict_group_: Failed to start dict group!\n";
        if (rc) *rc = -2;
    }
}

void json_start_list_group_(const char* name, int* rc, FORTRAN_LEN _name_sz)
{
    if (rc) *rc = 0;
    if (!currentStorageHandler) {
        std::cerr << "In json_start_list_group_: No active event storage!\n";
        if (rc) *rc = -1;
        return;
    }
    if (!currentStorageHandler->startGroup(JsonStorage::LIST, stdString(name, _name_sz))) {
        std::cerr << "In json_start_list_group_: Failed to start list group!\n";
        if (rc) *rc = -2;
    }
}

void json_end_group_(int* rc)
{
    if (rc) *rc = 0;
    if (!currentStorageHandler) {
        std::cerr << "In json_end_group_: No active event storage!\n";
        if (rc) *rc = -1;
    }
    if (!currentStorageHandler->endGroup()) {
        std::cerr << "In json_end_group_: Failed to end group!\n";
        if (rc) *rc = -2;
    }
}

void json_add_string_(const char* name, const char* value, int* rc, FORTRAN_LEN _name_sz, FORTRAN_LEN _value_sz)
{
    if (rc) *rc = 0;
    if (!currentStorageHandler) {
        std::cerr << "In json_add_string_: No active event storage!\n";
        if (rc) *rc = -1;
        return;
    }
    if (!currentStorageHandler->addVariable(stdString(name, _name_sz), stdString(value, _value_sz))) {
        std::cerr << "In json_add_string_(" << stdString(name, _name_sz) << ", " << stdString(value, _value_sz) << "): Failed to add value!\n";
        if (rc) *rc = -2;
    }
}

void json_add_double_(const char* name, const double* value, int* rc, FORTRAN_LEN _name_sz)
{
    if (rc) *rc = 0;
    if (!currentStorageHandler) {
        std::cerr << "In json_add_double_: No active event storage!\n";
        if (rc) *rc = -1;
        return;
    }
    if (!currentStorageHandler->addVariable(stdString(name, _name_sz), *value)) {
        std::cerr << "In json_add_double_(" << stdString(name, _name_sz) << ", " << *value << "): Failed to add value!\n";
        if (rc) *rc = -2;
    }
}

void json_add_int_(const char* name, const int* value, int* rc, FORTRAN_LEN _name_sz)
{
    if (rc) *rc = 0;
    if (!currentStorageHandler) {
        std::cerr << "In json_add_int_: No active event storage!\n";
        if (rc) *rc = -1;
        return;
    }
    if (!currentStorageHandler->addVariable(stdString(name, _name_sz), *value)) {
        std::cerr << "In json_add_int_(" << stdString(name, _name_sz) << ", " << *value << "): Failed to add value!\n";
        if (rc) *rc = -2;
    }
}

void json_add_bool_(const char* name, const bool* value, int* rc, FORTRAN_LEN _name_sz)
{
    if (rc) *rc = 0;
    if (!currentStorageHandler) {
        std::cerr << "In json_add_bool_: No active event storage!\n";
        if (rc) *rc = -1;
        return;
    }

    if (!currentStorageHandler->addVariable(stdString(name, _name_sz), *value)) {
        std::cerr << "In json_add_bool_(" << stdString(name, _name_sz) << ", " << *value << "): Failed to add value!\n";
        if (rc) *rc = -2;
    }
}

void json_add_double_vector_(const char* name, const double* values, int* values_size, int* rc, FORTRAN_LEN _name_sz)
{
    if (rc) *rc = 0;
    if (!currentStorageHandler) {
        std::cerr << "In json_add_double_values_: No active event storage!\n";
        if (rc) *rc = -1;
        return;
    }
    if (!currentStorageHandler->addVariable(stdString(name, _name_sz), values, static_cast<size_t>(*values_size))) {
        std::cerr << "In json_add_double_values_(" << stdString(name, _name_sz) << ", vector*" << *values_size << "): Failed to add values!\n";
        if (rc) *rc = -2;
    }
}

void json_add_int_vector_(const char* name, const int* values, int* values_size, int* rc, FORTRAN_LEN _name_sz)
{
    if (rc) *rc = 0;
    if (!currentStorageHandler) {
        std::cerr << "In json_add_int_values_: No active event storage!\n";
        if (rc) *rc = -1;
        return;
    }
    if (!currentStorageHandler->addVariable(stdString(name, _name_sz), values, static_cast<size_t>(*values_size))) {
        std::cerr << "In json_add_int_values_(" << stdString(name, _name_sz) << ", vector*" << *values_size << "): Failed to add values!\n";
        if (rc) *rc = -2;
    }
}

