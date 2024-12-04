% clear all;
% close all;
figure(3);
clf;
% Create some random data
s = [2 2 2];
x = randn(1000,1);
y1 = normrnd(s(1).*x,1);
y2 = normrnd(s(2).*x,1);
y3 = normrnd(s(3).*x,1);

data = [y1 y2 y3];
data = bsxfun(@minus, data, mean(data,1));

% Calculate the eigenvectors and eigenvalues
covariance = cov(data);

[u s v] = svd(covariance);


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

a=chisquare_val*sqrt(largest_eigenval);
b=chisquare_val*sqrt(middle_eigenval);
c=chisquare_val*sqrt(smallest_eigenval);

% the ellipse in x and y coordinates 
ellipse_x_r  = a*cos( alpha_grid );
ellipse_y_r  = b*sin(alpha_grid);
ellipse_z_r  = c*sin( gamma_grid );

%Projection operator on XY
% v = [largest_eigenvec middle_eigenvec smallest_eigenvec];
R12 = [cos(phi12) sin(phi12) 0; sin(phi12) -cos(phi12) 0; 0 0 0];


r_ellipse = (R12*[ellipse_x_r;ellipse_y_r;ellipse_z_r])';

% Draw the error ellipse
plot3(r_ellipse(:,1) + X0,r_ellipse(:,2) + Y0,r_ellipse(:,3) + Y1,'-b')
hold on;

% Plot the original data
plot3(data(:,1), data(:,2), data(:,3), '.');
mindata = min(min(data));
maxdata = max(max(data));
xlim([mindata-3, maxdata+3]);
ylim([mindata-3, maxdata+3]);
daspect([1 1 1]);
hold on;

% Plot the eigenvectors
% quiver3(X0, Y0, Y1, largest_eigenvec(1)*sqrt(largest_eigenval), largest_eigenvec(2)*sqrt(largest_eigenval), largest_eigenvec(3)*sqrt(largest_eigenval),'-r', 'LineWidth',2);
% quiver3(X0, Y0, Y1, smallest_eigenvec(1)*sqrt(smallest_eigenval), smallest_eigenvec(2)*sqrt(smallest_eigenval), smallest_eigenvec(3)*sqrt(smallest_eigenval),'-g', 'LineWidth',2);
% quiver3(X0, Y0, Y1, middle_eigenvec(1)*sqrt(middle_eigenval), middle_eigenvec(2)*sqrt(middle_eigenval), middle_eigenvec(3)*sqrt(middle_eigenval),'-b', 'LineWidth',2);
% hold on;

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

