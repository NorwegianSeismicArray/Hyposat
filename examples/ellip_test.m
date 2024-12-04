clear all;
close all;
%figure(3);
%clf;
%hold;
% Create some random data

v(1,3) = -0.19335805;
v(2,3) =  0.68781201;
v(3,3) =  0.69966228;
v(1,1) =  0.97706422;
v(2,1) =  0.19983067;
v(3,1) =  0.07357450;
v(1,2) = -0.08920856;
v(2,2) =  0.69784121;
v(3,2) = -0.71067537;
s(1,3) = 2.59900949 ;
s(2,1) = 0.23109584 ;
s(3,2) = 0.16755044 ;

tv = v(:,1);
ta = s(1,1);

xv = v(:,2);
xa = s(2,2);

yv = v(:,3);
ya = s(3,3);

%sum(xv .* tv);
%sum(yv .* tv);
%sum(xv .* yv);

%m4 = [1 0 0 ; 0 1 0; 0 0 1] ;
%phi1 = acos(sum(tv.*m4(:,1)));
%phi2 = acos(sum(tv.*m4(:,2)));
%phi3 = acos(sum(tv.*m4(:,3)));

% rotation of lon axes
%phi1 = acos(tv(1));
%m1 =  [cos(phi1) -sin(phi1) 0; sin(phi1) cos(phi1) 0; 0 0 -1];
phi1 =  -acos(tv(3));
%m2 =  [cos(phi1) -sin(phi1) 0; sin(phi1) cos(phi1) 0; 0 0 -1];
m2 =  [cos(phi1) sin(phi1) 0; -sin(phi1) cos(phi1) 0; 0 0 -1];
%phi1 =  acos(tv(3));
%m3 =  [cos(phi1) -sin(phi1) 0; sin(phi1) cos(phi1) 0; 0 0 -1];

v21 = ( v * m2 ) 

% rotation of lat axes
%phi2 = acos(tv(1));
%m1 =  [cos(phi2) 0 sin(phi2); 0 -1 0; -sin(phi2) 0 cos(phi2)];
%phi2 = acos(tv(2));
%m2 =  [cos(phi2) 0 sin(phi2); 0 -1 0; -sin(phi2) 0 cos(phi2)];
%phi2 = acos(tv(3));
%m2 =  [cos(phi2) 0 sin(phi2); 0 -1 0; -sin(phi2) 0 cos(phi2)];

phi2 = -acos(tv(2));
%m1 =  [cos(phi2) 0 sin(phi2); 0 -1 0; -sin(phi2) 0 cos(phi2)];
m1 =  [cos(phi2) 0 sin(phi2); 0 -1 0; -sin(phi2) 0 cos(phi2)];

v22 = ( v21 * m1 )

phi3 = -acos(tv(1));
m3 =  [cos(phi1) sin(phi1) 0; -sin(phi1) cos(phi1) 0; 0 0 -1];

% rotation of t axes
%phi3 = acos(tv(1));
%m1=  [ -1 0 0; 0 cos(phi3) -sin(phi3); 0 sin(phi3) cos(phi3)];
%phi3 = acos(tv(2));
%m2=  [ -1 0 0; 0 cos(phi3) -sin(phi3); 0 sin(phi3) cos(phi3)];
%phi3 = acos(tv(3));
%m3=  [ -1 0 0; 0 cos(phi3) -sin(phi3); 0 sin(phi3) cos(phi3)];

%phi3 = -acos(tv(2));
%m3=  [ -1 0 0; 0 cos(phi3) -sin(phi3); 0 sin(phi3) cos(phi3)];

v23 = ( v22 * m3 )

%mt = (v * m2 * m3 )
%mt = (v * m3 * m1 )
%mt = (v * m1 * m2 )
%mn = mt

 180/pi * acos(v)   - 90
 180/pi * acos(v21) - 90
 180/pi * acos(v22) - 90
 180/pi * acos(v23) - 90
% 180/pi * acos(mt)  - 90

sum(v23(:,1) .* v23(:,2))
sum(v23(:,1) .* v23(:,3))
sum(v23(:,3) .* v23(:,2))
