function res=simGaming(par)
if nargin<1
    par=initParameter;
end
%% init sim
y=zeros(par.N,1); % e2e delays per user input
ts=1/par.gameTicks*1e3; % time interval between game ticks (ms)
tr=1/par.frameTicks*1e3; % time interval between frame ticks (ms)

% first ticks are uniformly distributed in intervals
ts0=rand*ts; % (ms)
tr0=rand*tr; % (ms)
%% run sim
A=cumsum(exprnd(1/par.lambda, par.N,1)*1e3); % arrivals time points (ms)
F=cumsum(exprnd(1/par.lambda, par.N,1)); % finish time points (ms)
D=max(0,normrnd(par.net.mean,par.net.std,par.N,2)); % (ms) network delay up D(:,1) and down D(:,2)
B=max(0,normrnd(par.server.mean,par.server.std,par.N,1)); % (ms) processing delay
W=zeros(par.N,3); % waiting times (ms) due to discretization
for i=1:par.N
    nextTickR = ceil((A(i)-tr0)/tr)*tr+tr0; % waiting for input and processing in next tick
    W(i,1)=nextTickR - A(i); % (ms)
    arrivalAtServer = nextTickR+D(i,1); % uplink

    nextTickS = ceil((arrivalAtServer-ts0)/ts)*ts+ts0; % processing input at next game tick
    W(i,2)=nextTickS - arrivalAtServer; % (ms)

    arrivalAtClient = nextTickS + B(i) + D(i,2);
    displayTickR = (ceil((arrivalAtClient-tr0)/tr)+1)*tr+tr0; % render frame in next-next frame
    W(i,3)=displayTickR-arrivalAtClient;

    F(i)=displayTickR;
    
end

res.y=F-A;
res.F=F;
res.A=A;
res.B=B;
res.D=D;
res.W=W;
res.ts0=ts0;
res.ts=ts;
res.tsi=ceil((max(F)-ts0)/ts); % number of ticks

res.tr0=tr0; % first tick
res.tr=tr; % tick interval
res.tri=ceil((max(F)-tr0)/tr); % number of ticks




