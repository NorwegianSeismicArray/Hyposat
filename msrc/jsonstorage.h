#ifndef JSON_STORAGE_H
#define JSON_STORAGE_H

#include <map>
#include <vector>
#include <stack>
#include <string>
#include <ostream>


#ifdef WIN32
#define FORTRAN_LEN size_t
#else
#define FORTRAN_LEN int
#endif

/*!
 * \brief The JsonStorage class handles storing json formatted output
 */
class JsonStorage
{
public:
    enum GroupType {
        DICT=0,
        LIST
    };
    JsonStorage(std::ostream& stream, int precision=12);

    bool start(GroupType type);
    bool end();
    bool startGroup(GroupType type, const std::string& name);
    bool endGroup();
    bool addVariable(const std::string& name, const std::string& value);
    bool addVariable(const std::string& name, double value);
    bool addVariable(const std::string& name, int value);
    bool addVariable(const std::string& name, bool value);
    bool addVariable(const std::string& name, const double* values, size_t size);
    bool addVariable(const std::string& name, const int* values, size_t size);

private:
    bool handleFirstElementInGroup();
    bool          m_firstElement = true;
    GroupType     m_topGroupType = DICT;
    std::ostream& m_oStream;
    std::stack<std::string> m_groupNameStack;
    std::stack<GroupType> m_groupTypeStack;
    std::map<std::string, bool> m_firstElementMap;

};

extern "C" {
void json_start_list_(const char* filename, int* precision, int* rc, FORTRAN_LEN _filename_sz);
void json_start_dict_(const char* filename, int* precision, int* rc, FORTRAN_LEN _filename_sz);
void json_end_(int* rc);
void json_start_dict_group_(const char* name, int* rc, FORTRAN_LEN _name_sz);
void json_start_list_group_(const char* name, int* rc, FORTRAN_LEN _name_sz);
void json_end_group_(int* rc);
void json_add_string_(const char* name, const char* value, int* rc, FORTRAN_LEN _name_sz, FORTRAN_LEN _value_sz);
void json_add_double_(const char* name, const double* value, int* rc, FORTRAN_LEN _name_sz);
void json_add_int_(const char* name, const int* value, int* rc, FORTRAN_LEN _name_sz);
void json_add_bool_(const char* name, const bool* value, int* rc, FORTRAN_LEN _name_sz);
void json_add_double_vector_(const char* name, const double* values, int* values_size, int* rc, FORTRAN_LEN _name_sz);
void json_add_int_vector_(const char* name, const int* values, int* values_size, int* rc, FORTRAN_LEN _name_sz);

}

#endif // JSON_STORAGE_H
