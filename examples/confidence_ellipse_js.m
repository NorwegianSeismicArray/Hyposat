 clear all;
 close all;
figure(3);
clf;
% Create some random data
s = [1 2 3];
x = randn(1000,1);
y1 = normrnd(s(1).*x,1);
y2 = normrnd(s(2).*x,1);
y3 = normrnd(s(3).*x,1);

data = [y1 y2 y3];
data = bsxfun(@minus, data, mean(data,1));

%%%covariance (1,1) = 0.49295262951053986;
%%%covariance (1,2) = -1.1041786693810078;
%%%covariance (1,3) = -1.1271866935107637;
%%%covariance (2,1) = 0.;
%%%covariance (2,2) = 4.6631082162613923E-006;
%%%covariance (2,3) = 4.7602744714468406E-006;
%%%covariance (3,1) = 0.;
%%%covariance (3,2) = 0.15179244040993042;    
%%%covariance (3,3) = -6.0080879950347957E-002;
%%%covariance (4,1) = 0.24647631475526993;
%%%covariance (4,2) = -1.4012183008461168;    
%%%covariance (4,3) = -1.4304158078899687;      
%%%covariance (5,1) = 0.;
%%%covariance (5,2) = 2.0602651198807181E-004;
%%%covariance (5,3) = 2.1031953366168224E-004;  
%%%covariance (5,1) = 0.;
%%%covariance (5,2) = 7.5896220204965212E-002;
%%%covariance (5,3) = -3.0040439975173978E-002; 

% Calculate the eigenvectors and eigenvalues
covariance = cov(data);

[u s v] = svd(covariance);

%u
%s
%v

%% v(1,1) = -0.19335805;
%% v(2,1) =  0.68781201;
%% v(3,1) =  0.69966228;
%% v(1,2) =  0.97706422;
%% v(2,2) =  0.19983067;
%% v(3,2) =  0.07357450;
%% v(1,3) = -0.08920856;
%% v(2,3) =  0.69784121;
%% v(3,3) = -0.71067537;
%% s(1,1) = 2.59900949 ;
%% s(2,2) = 0.23109584 ;
%% s(3,3) = 0.16755044 ;

%% s
%% v

% Get the largest eigenvector and eigenvalue 
largest_eigenvec = v(:,1);
largest_eigenval = s(1,1);

% Get the second largest eigenvector and eigenvalue

middle_eigenvec = v(:,2);
middle_eigenval = s(2,2);

% Get the smallest eigenvector and eigenvalue

smallest_eigenvec = v(:,3);
smallest_eigenval = s(3,3);

% Calculate the angle between the x-axis and the largest eigenvector
phi12 = atan2(largest_eigenvec(2), largest_eigenvec(1));
phi13 = atan2(largest_eigenvec(3), largest_eigenvec(1));
phi23 = atan2(largest_eigenvec(3), largest_eigenvec(2));


% This angle is between -pi and pi.
% Let's shift it such that the angle is between 0 and 2pi
if(phi12 < 0)
    phi12 = phi12 + 2*pi;
end

if(phi13 < 0)
    phi13 = phi13 + 2*pi;
end

if(phi23 < 0)
    phi23 = phi23 + 2*pi;
end

ans = phi12 * 180/pi 
ans = phi13 * 180/pi 
ans = phi23 * 180/pi 

% Get the coordinates of the data mean
avg = mean(data);

% Get the 95% confidence interval error ellipse
chisquare_val = 2.832;
alpha_grid = linspace(-pi,pi);
beta_grid = linspace(-pi/2,pi/2);
gamma_grid = linspace(-pi/2,pi/2);

X0=avg(1);
Y0=avg(2);
Y1=avg(3);

%ans = sqrt(largest_eigenval)
%ans = sqrt(middle_eigenval)
%ans = sqrt(smallest_eigenval)

a=chisquare_val*sqrt(largest_eigenval) ;
b=chisquare_val*sqrt(middle_eigenval) ;
c=chisquare_val*sqrt(smallest_eigenval) ;
%

% the ellipse in x and y coordinates 
ellipse_x_r  = a*cos( alpha_grid );
ellipse_y_r  = b*sin(alpha_grid);
ellipse_z_r  = c*sin( alpha_grid );

phi12 = 0;
%Projection operator on XY
% v = [largest_eigenvec middle_eigenvec smallest_eigenvec];
R12 = [cos(phi12) sin(phi12) 0; sin(phi12) -cos(phi12) 0; 0 0 0];
R13 = [cos(phi13) sin(phi13) 0; sin(phi13) -cos(phi13) 0; 0 0 0];
R23 = [cos(phi23) sin(phi23) 0; sin(phi23) -cos(phi23) 0; 0 0 0];

alpha = atan2(v(1,3),-v(2,3));
alpha*180/pi
alpha = 0;
beta  = acos (v(3,3));
beta*180/pi
%beta = 0;
gamma = atan2(v(3,1),v(3,2));
gamma*180/pi
%gamma = 0;

%if(alpha < 0) 
%alpha = alpha + 2*pi ;
%end
%if(beta < 0) 
%beta = beta + 2*pi ;
%end
%if(gamma < 0) 
%gamma = alpha + 2*pi ;
%end
%if(alpha >= 2*pi) 
%alpha = alpha - pi ;
%end
%if(beta >= 2*pi) 
%beta = beta - pi ;
%end
%if(gamma >= 2*pi) 
%gamma = gamma - pi ;
%end


%zxz 
rot = [(cos(alpha)*cos(gamma)-cos(beta)*sin(alpha)*sin(gamma)) (-cos(alpha)*sin(gamma)-cos(beta)*cos(gamma)*sin(alpha)) sin(alpha)*sin(beta);
       (cos(gamma)*sin(alpha)+cos(alpha)*cos(beta)*sin(gamma)) (cos(alpha)*cos(beta)*cos(gamma)-sin(alpha)*sin(gamma)) -cos(alpha)*sin(beta);
        sin(beta)*sin(gamma) cos(gamma)*sin(beta) cos(beta) ]

v * rot'
v * R12

%rot= [cos(gamma) -sin(gamma) 0;
%      cos(beta)*sin(gamma) cos(beta)*cos(gamma) -sin(beta);
%      sin(beta)*sin(gamma) cos(gamma)*sin(beta) cos(beta) ]'

% Draw the error ellipse
r_ellipse = (R12*[ellipse_x_r;ellipse_y_r;ellipse_z_r])';
plot3(r_ellipse(:,1) + X0,r_ellipse(:,2) + Y0,r_ellipse(:,3) + Y1,'-b');
hold on

r0 = ([ellipse_x_r;ellipse_y_r;ellipse_z_r]'*rot);
plot3(r0(:,1) + X0,r0(:,2) + Y0,r0(:,3) + Y1,'-r')
hold on

% Plot the original data
%plot3(data(:,1), data(:,2), data(:,3), '.')'';
mindata = min(min(data));
maxdata = max(max(data));
xlim([mindata-3, maxdata+3]);
ylim([mindata-3, maxdata+3]);
daspect([1 1 1]);
hold on;

% c=chisquare_val*(smallest_eigenval);
% c=chisquare_val*(smallest_eigenval);

% ellipsoid(0.,0.,0.,c,b,a)


% Plot the eigenvectors
quiver3(X0, Y0, Y1, largest_eigenvec(1)*sqrt(largest_eigenval), largest_eigenvec(2)*sqrt(largest_eigenval), largest_eigenvec(3)*sqrt(largest_eigenval),'-r', 'LineWidth',2);
quiver3(X0, Y0, Y1, smallest_eigenvec(1)*sqrt(smallest_eigenval), smallest_eigenvec(2)*sqrt(smallest_eigenval), smallest_eigenvec(3)*sqrt(smallest_eigenval),'-g', 'LineWidth',2);
quiver3(X0, Y0, Y1, middle_eigenvec(1)*sqrt(middle_eigenval), middle_eigenvec(2)*sqrt(middle_eigenval), middle_eigenvec(3)*sqrt(middle_eigenval),'-b', 'LineWidth',2);
hold on;

% Set the axis labels
hXLabel = xlabel('x');
hYLabel = ylabel('y');


% r_ellipse = [ellipse_x_r;ellipse_y_r;ellipse_z_r]' * R12;
% plot3(r_ellipse(:,1) + X0,r_ellipse(:,2) + Y0,r_ellipse(:,3) + Y1,'-r')
% r_ellipse = [ellipse_x_r;ellipse_y_r;ellipse_z_r]' * R13;
% plot3(r_ellipse(:,1) + X0,r_ellipse(:,2) + Y0,r_ellipse(:,3) + Y1,'-g')
% r_ellipse = [ellipse_x_r;ellipse_y_r;ellipse_z_r]' * R23;
% plot3(r_ellipse(:,1) + X0,r_ellipse(:,2) + Y0,r_ellipse(:,3) + Y1,'-b')
% daspect([1 1 1]);


% For a specific level of standard deviation, scale the covariance matrix:
% 
% STD = 2;                     %# 2 standard deviations
% conf = 2*normcdf(STD)-1;     %# covers around 95% of population
% scale = chi2inv(conf,2);     %# inverse chi-squared with dof=#dimensions
% 
% Cov = cov(X0) * scale;
% [V D] = eig(Cov);

