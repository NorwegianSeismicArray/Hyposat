clear all;
close all;
%figure(3);
%clf;
%hold;
% Create some random data

%        time          x           y           z
v = [  -0.70813724  0.03214610  0.68783713  0.15616771;
       -0.02118476  0.71270546 -0.20719749  0.66982933;
       -0.00243823 -0.69885096 -0.12955769  0.70343173;
        0.70575272  0.05123378  0.68349403  0.17923200]

s = [ 0.07928786 0.47096121 0.90409754 0.75502160 ];

% tx-rotation

a = atan2(v(4,3),-v(4,4));
if(a < -pi) 
a = a + pi ;
end
if(a > pi) 
a = a - pi ;
end
a*180/pi

rot1 = [ 1 0 0 0;
        0 1 0 0 ;
        0 0 cos(a) -sin(a);
        0 0 sin(a) cos(a) ] ;
r1 = v * rot1

% ty-rotation

b = atan2(r1(4,2),-r1(4,4));
if(b < -pi) 
b = b + pi ;
end
if(b > pi) 
b = b - pi ;
end
b*180/pi

rot2 = [ 1 0 0 0;
        0 cos(b) 0 -sin(b);
        0 0 1 0 ;
        0 sin(b) 0 cos(b) ] ;
r2 = r1 * rot2

% xy-rotation

c = atan2(r2(4,1),-r2(4,4));
if(c < -pi) 
c = c + pi ;
end
if(c > pi) 
c = c - pi ;
end
c*180/pi

rot3= [ cos(c) 0 0 -sin(c);
        0 1 0 0;
        0 0 1 0 ;
        sin(c) 0 0 cos(c) ] ;
r3 = r2 * rot3

%sum(r0(:,1) .* r0(:,2))
%sum(r0(:,1) .* r0(:,2))
%sum(r0(:,1) .* r0(:,3))
%sum(r0(:,3) .* r0(:,2))

%r3 = [ -0.19335805  0.97706422 -0.08920856  0 ;
%        0.68781201  0.19983067  0.69784121  0 ;
%        0.69966228  0.07357450 -0.71067537  0 ;
%         0 0 0 0 ]

% tx rotation

d = atan2(r3(1,1),-r3(1,2))
if(d < -pi) 
d = d + pi ;
end
if(d > pi) 
d = d - pi ;
end
d*180/pi

rot4 = [ -sin(d) cos(d) 0 0 ;
          cos(d) sin(d) 0 0 ;
           0      0     1 0 ;
           0      0     0 1 ] ;
r4 = r3*rot4
         
% ty rotation

e = atan2(r4(1,1),-r4(1,3))
if(e < -pi/2) 
e = e + pi ;
end
if(e > pi/2) 
e = e - pi ;
end
e*180/pi

rot5 = [ -sin(e) 0 cos(e) 0 ;
           0     1    0   0 ;
          cos(e) 0 sin(e) 0 ;
           0      0     0 1 ] ;
r5 = r4*rot5
         
%l(:,1) = s(1) * r5(:,1);
%l(:,2) = s(2) * r5(:,2);
%l(:,3) = s(3) * r5(:,3);
%l(:,4) = s(4) * r5(:,4);
%l
