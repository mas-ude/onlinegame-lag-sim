clear all;clc;
%% run sim
par=initParameter('N',1);
res=simGaming(par);
% visualization parameters
len = 0.4; % length of ticks
server.y=1;server.col=[0 0.5 0];
client.y=0;client.col=[0 0 0.5];
input.col=[0.5 0 0];
%
figure(1);clf;
% client ticks
stem(res.tr0+res.tr*(0:res.tri),client.y+ones(res.tri+1,1)*len-len/2,...
    'color',client.col,'Marker','none',...
    'basevalue',client.y-len/2,'showbaseline','off');
hold all;
% server ticks
stem(res.ts0+res.ts*(0:res.tsi),server.y+ones(res.tsi+1,1)*len-len/2,...
    'color',server.col,'Marker','none','basevalue',server.y-len/2,'showbaseline','off');

% lines per input
for i=1:par.N
    % arrival + nextTick + uplink + nextGameTick + serverProcessing +
    % nextTick + nextTick
    t=cumsum([res.A(i) res.W(i,1) res.D(i,1) res.W(i,2) res.B(i) res.D(i,2) res.W(i,3)]);
    y=[client.y client.y server.y server.y server.y client.y client.y];
    plot(t,y,'o-');
end
ylim([client.y-1 server.y+1])
xlabel('time (ms)')
set(gca,'ytick',[client.y server.y],'yticklabel',{'client','server'})
xlim([0 max(res.F)+res.tr]);

%% run exemplary sim runs with two different frame ticks (Hz)
N=1e4;
par1=initParameter('N',N,'frameTicks',30);
res1=simGaming(par1);
par2=initParameter('N',N,'frameTicks',60);
res2=simGaming(par2);
%%
figure(2);clf;
cdfplot(res1.y);
hold all
cdfplot(res2.y);
xlabel('e2e delay (ms)')
ylabel('CDF')
legend(sprintf('F=%d Hz',par1.frameTicks),...
    sprintf('F=%d Hz',par2.frameTicks),4);
title(''); grid off;


%%
Hz=30:5:160;N=1e4;
y=zeros(N,length(Hz));
for i=1:length(Hz)
    par=initParameter('N',N,'frameTicks',Hz(i));
    res=simGaming(par);
    y(:,i)=res.y;
end
%%
figure(3);clf;
boxplot(y,'labels',num2str(Hz'))
hold all;
plot(mean(y),'kd');
xlabel('frame ticks (Hz)')
ylabel('e2e delay (ms)')
%%
figure(4);clf;
plot(Hz,mean(y),'kd');
xlabel('frame ticks (Hz)')
ylabel('mean e2e delay (ms)')
hold all
f=@(p,x) p(1)*exp(-p(2)*x)+p(3);
p=nlinfit( Hz,mean(y),f,[50 0.01 120]);
x=min(Hz):max(Hz);
plot(x,f(p,x),'g-')

%f2=@(p,x) p(1)+p(2)./(x-p(3));
%p2=nlinfit( Hz,mean(y),f2,[120 -1 31]);
f2=@(p,x) p(1)+p(2)./(x+p(3));
p2=nlinfit( Hz,mean(y),f2,[55 2000 0]);

x=min(Hz):max(Hz);
plot(x,f2(p2,x),'r-')

legend('Simulation',...
    sprintf('f_1(x)=%.1f exp(-%.4fx) + %.1f',p),...
    sprintf('f2(x)=%.2f%+.2f/(x%+.2f)',p2));
xlim([min(Hz) max(Hz)]);

%%
figure(5);clf;
plot(1e3./Hz,mean(y),'kd');
xlabel('tick duration (ms)')
ylabel('mean e2e delay (ms)')
hold all

f3=@(p,x)p(1)*x+p(2);
p3=nlinfit( 1e3./Hz,mean(y),f3,[55 10]);
plot(1e3./x,f3(p3,1e3./x),'r-')

legend('Simulation',...
    sprintf('f2(x)=%.2f%+.2f/(x%+.2f)',p2),4);
%xlim([min(Hz) max(Hz)]);
%set(gca,'yscale','lin','xscale','log')
%%
Hz=30:5:160;N=1e6;
gameTicks = [ 10 20 40 60];
z=zeros(length(Hz),length(gameTicks));
sos=zeros(length(Hz),length(gameTicks));
for j=1:length(gameTicks)
    for i=1:length(Hz)
        par=initParameter('N',N,'frameTicks',Hz(i),'gameTicks',gameTicks(j));
        res=simGaming(par);
        z(i,j)=mean(res.y);
        sos(i,j)=std(res.y);
    end
end
%%
figure(6);clf;
plot(1e3./Hz,z)
xlabel('frame tick length (ms)')
ylabel('mean e2e delay (ms)')
legend(num2str(gameTicks','Game Ticks %d (Hz)'),4);
%%
figure(8);clf;
plot(Hz,sos)
xlabel('frame tick (Hz)')
ylabel('STD e2e delay (ms)')
legend(num2str(gameTicks','Game Ticks %d (Hz)'),'location','best');
%%
figure(7);clf;
Hz=30:5:160;N=1e5;
gameTicks = [ 30 60 ];
z=zeros(length(Hz),length(gameTicks));
netstd=[5 20 40];
style={'--','-',':','.-'};
cols=hsv(length(gameTicks));
for k=1:length(netstd)
    for j=1:length(gameTicks)
        for i=1:length(Hz)
            par=initParameter('N',N,...
                'frameTicks',Hz(i),'gameTicks',gameTicks(j));
            par.net.std=netstd(k);
            res=simGaming(par);
            z(i,j)=mean(res.y);
        end
        plot(1e3./Hz,z(:,j),'color',cols(j,:),'linestyle',style{k});
        hold all;
    end
end
xlabel('frame tick length (ms)')
ylabel('mean e2e delay (ms)')
for k=1:length(netstd)
    h(k)=plot(nan,nan,'k','linestyle',style{k});
end;
legend(h,num2str(netstd','\\sigma_{net}=%d (ms)'),4)

ax1=gca;
ax2 = axes('Position',get(ax1,'Position'),...           
    'Color','none',...
    'XColor','k','YColor','k','visible','off');

for j=1:length(gameTicks)
    hh(j)=plot(1,1,'Color',cols(j,:),'linestyle','-');hold all
end
set(ax2,'visible','off')
legend(num2str(gameTicks','Game Ticks %d (Hz)'),2)


%%
for i=1:8, save2pdf(num2str(i,'fig%i'),i,300,12,1.5);end