clear all;
close all;
%figure(3);
%clf;
%hold;
% Create some random data

v(1,1) = -0.19335805;
v(2,1) =  0.68781201;
v(3,1) =  0.69966228;
v(1,2) =  0.97706422;
v(2,2) =  0.19983067;
v(3,2) =  0.07357450;
v(1,3) = -0.08920856;
v(2,3) =  0.69784121;
v(3,3) = -0.71067537;
s(1,1) = 2.59900949 ;
s(2,2) = 0.23109584 ;
s(3,3) = 0.16755044 ;

beta  = acos (v(1,1));
gamma = atan2(v(1,2),v(1,3));
beta*180/pi
gamma*180/pi

%if(alpha < -pi) 
%alpha = alpha + pi ;
%end
%if(alpha > pi) 
%alpha = alpha - pi ;
%end
%alpha*180/pi
alpha = 0;

if(gamma < -pi) 
gamma = gamma + pi ;
end
if(alpha > pi) 
gamma = gamma - pi ;
end

beta
if(beta < -pi/2) 
beta = beta + pi ;
end
if(beta >= pi/2) 
beta = beta - pi ;
end
beta

beta*180/pi
gamma*180/pi

beta
gamma

%zxz 
%rot1 = [(cos(alpha)*cos(gamma)-cos(beta)*sin(alpha)*sin(gamma)) (-cos(alpha)*sin(gamma)-cos(beta)*cos(gamma)*sin(alpha)) sin(alpha)*sin(beta);
%       (cos(gamma)*sin(alpha)+cos(alpha)*cos(beta)*sin(gamma)) (cos(alpha)*cos(beta)*cos(gamma)-sin(alpha)*sin(gamma)) -cos(alpha)*sin(beta);
%        sin(beta)*sin(gamma) cos(gamma)*sin(beta) cos(beta) ]
%xzx
rot2 = [ cos(beta) -cos(gamma)*sin(beta) sin(beta)*sin(gamma);
         cos(alpha)*sin(beta) (cos(alpha)*cos(beta)*cos(gamma)-sin(alpha)*sin(gamma)) (-cos(gamma)*sin(alpha)+cos(alpha)*cos(beta)*sin(gamma));
         sin(alpha)*sin(beta) (cos(alpha)*sin(gamma)+cos(beta)*cos(gamma)*sin(alpha)) (cos(alpha)*cos(gamma)-cos(beta)*sin(alpha)*sin(gamma)) ]
%xyx
rot3 = [ cos(beta) sin(beta)*sin(gamma) cos(gamma)*sin(beta);
            0         cos(gamma)              -sin(gamma);
         -sin(beta) cos(beta)*sin(gamma) cos(beta)*cos(gamma) ]

rot4 = [ cos(beta)                 0             -sin(beta)      ;
         sin(beta)*sin(gamma)    cos(gamma) cos(beta)*sin(gamma) ;
         cos(gamma)*sin(beta)   -sin(gamma) cos(beta)*cos(gamma) ]



%r0 = v * rot1'
r0 = v * rot3'
r0 = v * rot4
%(180/pi)*acos(v)-90
%(180/pi)*acos(r0)-90


sum(r0(:,1) .* r0(:,2))
sum(r0(:,1) .* r0(:,3))
sum(r0(:,3) .* r0(:,2))

