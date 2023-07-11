clc
clear all
num=xlsread('1.xlsx');
for i=0:19
    %10minheartbeat
a2=num(i*600+1:(i+1)*600,2);
len_a2=length(a2);
 
%figure,subplot(2,1,1)
%plot(a2),title(' heart beat for 1 min')
 
count=0; %double                       
for i2 = 1:length(a2)
    if i2<length(a2)-1;
    if 0<(a2(i2+1,:)-a2(i2,:))&& 0 > (a2(i2+2,:)-a2(i2+1,:))
        count = count+1;
       %hold on 
       % plot(i2+1, a2(i2+1),'r*')
    end
    end
 
end
countarray(i+1)=count;
 end
heartbeatcount=countarray/60;


