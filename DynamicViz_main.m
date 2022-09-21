% Function [DistMatrices,Y]=DynamicViz_main
%
% Distance-based dymanic data analysis and vizualization.
%
% This function reads three datasets from three excel files.
% Each dataset contains dayly data from 01/02/2021 to 14/03/2022 for 25 EU
% state members, regarding health, mobility and stringency variables.
%
% We conisder weekly data: t=1,...,q. 
% The function computes pairwse robust distances between countries for each
% week t, and represent their similarities in several line-plots, and
% MDS-maps.
%
% Some code lines are commented, mainly those correspnding to savig the
% results in excel files. Uncomment them if necessary.
%
% Inputs:  No inputs are required (datasets are read insdie the function)
% Outputs: DistMatrices = is a cubic structure containing a robust distance
%                          matrix D(t) in each slice t (one per week),
%          Y = is a cubic structure containing the principal cordinates of
%              D(t) in each slice t (one per week).

function [DistMatrices,Y]=DynamicViz_main
% We load the three excel files as table and convert them to timetable. 
% All variables are quantitative.

% We import the data from three Excel files
% Three sources of information. 25 countries
 T1=readtable('healthdata_25countries.xlsx');
 T2=readtable('mobilitydata_25countries.xlsx');
 T3=readtable('stringencydata_25countries.xlsx');

TT1_aux=table2timetable(T1);
TT2_aux=table2timetable(T2);
TT3_aux=table2timetable(T3);

TT1=TT1_aux(:,vartype('numeric'));
TT2=TT2_aux(:,vartype('numeric'));
TT3=TT3_aux(:,vartype('numeric'));
days=unique(TT1.date);
tmax=length(days);
vg1_aux=zeros(tmax,1);
vg2_aux=zeros(tmax,1);
vg3_aux=zeros(tmax,1);

varlabel={'health','mobility','stringency'};
% labels for 25 countries
label= {'ITA','ESP','AUT','BEL','BGR','CZE','DEU','DNK','EST','FIN','FRA','GRC','HRV','HUN','IRL','LTU','LVA','MLT','NLD','POL','PRT','ROU','SVN','SVK','SWE'};
% We use a cubic structure to save all distance matrices, that is,
% we store each D(t) in a dfferent slice t. 

% Computations for the first week
data1_aux=TT1(days(1),:);
data2_aux=TT2(days(1),:);
data3_aux=TT3(days(1),:);
data1=table2array(data1_aux);
data2=table2array(data2_aux);
data3=table2array(data3_aux);
n=size(data1,1); % the number of countries
% We compute robust Mahalanobis distance and each geometric variability
D1=robust2_maha(data1); vg1_aux(1)=sum(sum(D1))/n;
D2=robust2_maha(data2); vg2_aux(1)=sum(sum(D2))/n;
D3=robust2_maha(data3); vg3_aux(1)=sum(sum(D3))/n;

DistMatrices=D1/vg1_aux(1)+D2/vg2_aux(1)+D3/vg3_aux(1); % Is a nxn matrix. We add more slices into the loop.

% Computattions for the next weeks
for t=8:7:tmax
   data1_aux=TT1(days(t),:); data1=table2array(data1_aux);
   data2_aux=TT2(days(t),:); data2=table2array(data2_aux);
   data3_aux=TT3(days(t),:); data3=table2array(data3_aux);
   D1=robust2_maha(data1); vg1_aux(t)=sum(sum(D1))/n;
   D2=robust2_maha(data2); vg2_aux(t)=sum(sum(D2))/n;
   D3=robust2_maha(data3); vg3_aux(t)=sum(sum(D3))/n; 
   DistMatrices(:,:,1+(t-1)/7)=D1/vg1_aux(t)+D2/vg2_aux(t)+D3/vg3_aux(t);
end
q=size(DistMatrices,3); % the number of weeks

% We save DistMatrices in an excel file called DistMatrices_file.xlsx
%(it takes two minuts and gives some waringings, but it works!)
% Uncomment these code lines to save the q=59 distance matrices
% filename = 'DistMatrices_file.xlsx';
% for i=1:q
%  d=DistMatrices(:,:,i);
%  sheet=i;
%  xlswrite(filename,d,sheet)
% end
% clear d

%-------------------------------------------
% MDS for each Distance matrix in DistMatrices(:,:,i), (i=1,...,q)
for i=1:q
    [Y(:,:,i),acum(:,:,i)] = coorp_noplots2(DistMatrices(:,:,i));
end
% We save each MDS-map in a excel file called MDSmaps_file.xlsx
% Uncomment these code lines to save the q=59 MDS-maps 
% filename = 'MDSmaps_file.xlsx';
% for i=1:q
%  y=Y(:,:,i);
%  sheet=i;
%  xlswrite(filename,y,sheet)
% end
% clear y
%
% We save the percentage of explained variability of each MDS-map in an
% excel file called ExpalinedVar_file.xlx
% Uncomment these code lines to save the q=59 explained variabilities
% filename = 'ExpalinedVar_file.xlsx';
% for i=1:q
%  ev=acum(:,:,i);
%  sheet=i;
%  xlswrite(filename,ev,sheet)
% end
% clear ev
%-----------------------------------------------

k1=find(vg1_aux>0);
k2=find(vg2_aux>0);
k3=find(vg3_aux>0);
vg1=vg1_aux(k1); vg2=vg2_aux(k2); vg3=vg3_aux(k3);
clear vg1_aux vg2_aux vg3_aux

%==================================
% Dynamic visualization
%----------------------------------
% 1. We plot the geometric variability for each t
%
%  Rescaled version of the plot to 0-1 
figure
plot(vg1/max(vg1),'b-','LineWidth',1.5)
hold on
plot(vg2/max(vg2),'r--','LineWidth',1.5)
plot(vg3/max(vg3),'k:','LineWidth',1.5)
xline(25) % week 25th 
xline(50) % week 50th
ax = gca;
text(10,ax.YLim(1),'Gamma')
text(35,ax.YLim(1),'Delta')
text(51,ax.YLim(1),'Omicron')
title('Geometric variability')
xlabel('Time (week)')
legend(varlabel,'Location','best')
%
%----------------------------------
% % 2. We plot the distance of each country J to the others along time
% for J=1:n % for country J
%    for t=1:q
%      Y(:,t)=DistMatrices(J,:,t);
%    end
%    figure
%    plot(Y')
%    title('Distances of',label(J))
%    xlabel('Time (week)')
%    legend(label,'Location','bestoutside')
% end
%----------------------------------
% 3. We plot the proximity function of each country J to the others along time
% Proximity function phi along time
for J=1:n % for country J
   for t=1:q
     Dt=DistMatrices(:,:,t);
     Dti=Dt; Dti(J,:)=[]; Dti(:,J)=[];
     Vi(t,1)=sum(sum(Dti))/(2*(n-1)^2);
     % proximity function for country J to the others at time t
     phi(t,J)=sum(Dt(:,J))/(n-1)-Vi(t,1);
   end 
end
Threshold=prctile(phi',90);
figure
plot(phi)
hold on
plot(Threshold,'r-','LineWidth',1.5)
title('Proximity of each country to the others along time')
xlabel('Time (week)')
legend(label,'Location','bestoutside')

%---------------------------------
% 4. We plot the maximum pairwise distances along time
MaxDist=max(max(DistMatrices));
figure
plot(MaxDist(:),'b-')
hold on
% We find the pair at maximum distance and add it to the plot
[M,ind3]=max(DistMatrices,[],3);
[Mcol,ind2]=max(M,[],1);
[Mrow,ind1]=max(Mcol);
t0=ind3(ind1,ind2(ind1));
plot(t0,Mrow,'.b','MarkerSize',15)
text(t0,Mrow+0.005,label(ind1))
text(t0,Mrow-0.005,label(ind2(ind1)))
title('Maximum pairwise distances')
%----------------------------------
% 5. We plot the minimum pairwise distances along time
for t=1:q
  MinDist(t)=min(squareform(DistMatrices(:,:,t)));
end  
figure
plot(MinDist(:),'r-')
hold on
% We find the pair at minimum distance and add it to the plot
[M2,ind3b]=min(DistMatrices,[],3);
[Mcol2,ind2b]=min(M2+max(max(M2))*eye(size(M2)),[],1);
[Mrow2,ind1b]=min(Mcol2);
t1=ind3b(ind1b,ind2b(ind1b));
plot(t1,Mrow2,'.r','MarkerSize',15)
text(t1-2,Mrow2,label(ind1b))
text(t1+2,Mrow2,label(ind2b(ind1b)))
title('Minimum pairwise distances')
%----------------------------------
% 6. We plot the maximum/minimun pairwise distances along time 
figure
plot(MaxDist(:),'b-')
hold on
plot(MinDist(:),'r-')
plot(t0,Mrow,'.b','MarkerSize',15)
text(t0-2,Mrow+0.005,label(ind1))
text(t0+2,Mrow-0.005,label(ind2(ind1)))
plot(t1,Mrow2,'.r','MarkerSize',15)
text(t1-2,Mrow2,label(ind1b))
text(t1+2,Mrow2,label(ind2b(ind1b)))
title('Maximum and minimum pairwise distances')

%----------------------------------
% 7. We plot the maximum/minimun pairwise distances along time jointly with
% Q1 and Q3 pairwise distances
Q1Dist=prctile(prctile(DistMatrices,25),25);
Q3Dist=prctile(prctile(DistMatrices,75),75);
figure
plot(MaxDist(:),'b-','LineWidth',1.5)
hold on
plot(MinDist(:),'r-','LineWidth',1.5)
plot(Q3Dist(:),'k-.','LineWidth',1.5)
plot(Q1Dist(:),'k-.','LineWidth',1.5)
plot(t0,Mrow,'.b','MarkerSize',15)
text(t0-2,Mrow+0.005,label(ind1))
text(t0+2,Mrow-0.005,label(ind2(ind1)))
plot(t1,Mrow2,'.r','MarkerSize',15)
text(t1-2,Mrow2,label(ind1b))
text(t1+2,Mrow2,label(ind2b(ind1b)))
text(2,Q1Dist(2)-0.01,'Q1-pairwise distances')
text(2,Q3Dist(2)+0.01,'Q3-pairwise distances')
title('Dynamic box-plot')
clear M M2 Mcol Mcol2 Mrow Mrow2 t0 t1 ind1 ind1b ind2 ind2b ind3 ind3b 

end
%
% Function D=robust2_maha(X) computes the matrix of Mahalanobis distances
% (squared units) between the rows of X. A robust estimator is used
% for the covariance of X.
%
% Inputs:   X matrix nxp of quantitative data.
% Outputs:  D matrix nxn of distances (squared units).
%
function D=robust2_maha(X)
[n,p]=size(X);
tol=1e-8;

% naive version
if min(max(X)-min(X))<tol
    X=X+randn(size(X))*1e-5;
else
end

S=zeros(p,p);
        for jj=20:-1:0
            m=trimmean(X,jj);
            S=(X-ones(n,1)*m)'*(X-ones(n,1)*m)/n;
            if rank(S) ==p
                break
            end
        end

D=zeros(n,n);
for i=1:n
    D(i+1:n,i)=mahalFS(X(i+1:n,:),X(i,:),S);
end
D=D+D';

end

% COORP_NOPLOTS
%
% Function [Y,acum] = coorp_noplots(D) computes the principal coordinates 
% of a matrix D of squared distances,
%
% Inputs:  D = matrix of squared distances.
%
% Outputs:
%    Y = matrix of principal coordinates,
%    
%    acum = row vector with the cumulated percentage of explained
%    variability.
%
 function [Y,acum] = coorp_noplots2(D)
 n=size(D,1);
% we check if D fulfills the Euclidean property (ie, B>=0)
 H=eye(n)-ones(n)/n;
 B=-H*D*H/2;
 L=eig(B);
 m=min(L);
 epsilon=1.e-6;
 if abs(m) > epsilon
    % we apply the transformation non2euclid so that D fulfills the Euclidean property
    D1=non2euclid(D);
    B=-H*D1*H/2;
 end
%--------------------------------------------------
% computation of the principal coordinates (only those with non null eigenvalues)
 [T,Lambda,V]=svd(B);
 vaps=diag(Lambda)';
 nvaps=find(abs(vaps)>epsilon);
 T1=T(:,nvaps);
 % control de signos
 for j=1:size(T1,2)
   if T1(1,j)<0
      T1(:,j)=-T1(:,j);
   end
 end  
 Y=T1*sqrt(Lambda(nvaps,nvaps));
 percent=vaps/sum(vaps)*100;
 acum=zeros(1,n);
 for i=1:n
    acum(i)=sum(percent(1:i));
 end
%-----------------------------------------------------
 end

% non2euclid
%
% Given a matrix D (nxn) of squared distances, this function applyies a 
% transformation to D that preserves the ordering, and returns a matrix D1 
% of squared distances that fulfills the Euclidean property.
%
 function D1=non2euclid(D)
 [n,n]=size(D);
 H=eye(n)-ones(n)/n;
 [T,Lambda]=eig(-H*D*H/2);
 m=min(diag(Lambda));
 D1=D-2*m*ones(n)+2*m*eye(n);
 end
