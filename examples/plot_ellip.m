%

dlat = 0.0767;
dlon = 0.0703;

a= 0.89268373177668559         
b= 1.1390446726179391         

phi = 42.7876594442

sphir = sin(phi*pi/180);
cphir = cos(phi*pi/180);

for k = 1:360;

beta = k*pi/180;

eli(k,1) = a*sin(beta);
eli(k,2) = b*cos(beta);

elir(k,1) = eli(k,1)*cphir - eli(k,2)*sphir;
elir(k,2) = eli(k,1)*sphir + eli(k,2)*cphir;

end

clf
%plot(eli(:,1),eli(:,2),'r')
plot(elir(:,1),elir(:,2),'r')
hold

m(1) = abs(max(eli(:,1)));
m(2) = abs(max(eli(:,2)));
m(3) = abs(max(elir(:,1)));
m(4) = abs(max(elir(:,2)));
s = max(m);
p(1,1) = 0.;
p(1,2) = 0.;
p(2,1) = s;
p(2,2) = -s;
plot(p(1,:),p(2,:),'b')
p(1,1) = s;
p(1,2) = -s;
p(2,1) = 0.;
p(2,2) = 0.;
plot(p(1,:),p(2,:),'b')

% vector lat
p1(1,1) = 0.;
p1(1,2) = 5.6083390239787999E-002    
p1(2,1) = 0.;
p1(2,2) = 5.1950512755693179E-002    
plot(p1(1,:),p1(2,:),'m')

% vector lon
p2(1,1) = 0.;
p2(1,2) = -5.0095563128278980E-002      
p2(2,1) = 0.;
p2(2,2) = 4.9243633343229828E-002    
plot(p2(1,:),p2(2,:),'c')

sp = p1(2,2)*p2(2,2) + p1(1,2)*p2(1,2)

x(1) = dlon;
y(1) = max(elir(:,2));

x(2) = max(elir(:,1));
y(2) =  dlat;

p(1,1) = x(1);
p(1,2) = x(1);
p(2,1) = y(1);
p(2,2) = -y(1);
plot(p(1,:),p(2,:),'g')

p(1,1) = -x(1);
p(1,2) = -x(1);
p(2,1) =  y(1);
p(2,2) = -y(1);
plot(p(1,:),p(2,:),'g')

p(1,1) = x(2);
p(1,2) = -x(2);
p(2,1) = y(2);
p(2,2) = y(2);
plot(p(1,:),p(2,:),'g')

p(1,1) = x(2);
p(1,2) = -x(2);
p(2,1) = -y(2);
p(2,2) = -y(2);
plot(p(1,:),p(2,:),'g')


