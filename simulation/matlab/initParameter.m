function par=initParameter(varargin)
par.gameTicks=60; % Hz=1/s
par.frameTicks=60; % Hz=1/s
par.lambda=20; % user input / sec
par.net.mean=20; % one-way network delay (ms)
par.net.std=5; % owd standand deviation (ms)
par.server.mean=3; % mean game server delay (ms)
par.server.std=0.1; % mean game server delay (ms)
par.N=100; % number of simulated user inputs

if mod(nargin,2)
    error('Pass key/value pairs to function: %s(key,value)',mfilename);
end
for i = 1 : 2 : length(varargin)
    name = varargin{i};
    value = varargin{i+1};
    if ~isfield(par,name)
        warning('par.%s does not exist; ignoring %s=%g !',name,name,value);
    else
        par.(name)=value;
    end
end