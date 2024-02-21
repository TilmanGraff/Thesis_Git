
% This file takes adj, dist, and speed matrices and calculates current tradeflows, optiomal networks, and welfare gains

%% Read in global data and define parameters
addpath /Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/code/2_optimise/OptimalTransportNetworkToolbox/Code
addpath /Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/code/2_optimise/OptimalTransportNetworkToolbox/Ipopt-31

clear;

country_table = readtable("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/country_names.csv");
country_names = table2array(country_table);

centroids = readtable("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/centroids.csv", 'TreatAsEmpty',{'.','NA'}, 'EmptyValue', 0);
centroids.country = categorical(centroids.country);

% Defines parameters

alpha = 0.7;
gamma = 0.946;
beta = 1.2446 * gamma;
sigma = 5;
a = 1.0; % production function parameter, it doesnt matter because production is fixed anyway. Just have to make sure that its the same as in the define_productivity_and_rownames.R file
rho = 0; % this is really important to not have any inequality aversion. I am not entirely sure if thats legit because in their toolbox, FS say rho >= 1... but i see no reason why 0 should not be ok...

%% For each country
cs = strings(1);
pops = nan(1);
facts = nan(1);
nums = nan(1);

 for countryID = 1:length(country_names)
    
     countryname = (country_names(countryID))
     if exist(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/productivities/productivities_", (countryname), ".csv"))  %&& ~exist(strcat("//Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/Network_outcomes/", (countryname), "_outcomes.csv"))

        % Split centroids by country
        case_centroids = readtable(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/borderregions/", (countryname), "_borderregion.csv"));
        num_locations = size(case_centroids, 1)
        if countryname == "Nigeria" || countryname == "Ethiopia"
        %if countryname == "Eritrea"
        
        % Read in characteristics
        population = case_centroids.pop;


        % Read in the relevant matrices
        adj = csvread(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/adj/adj_", (countryname), ".csv"), 1, 0);
        abr = csvread(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/abr/abr_", (countryname), ".csv"), 1, 0);
        delta_I = csvread(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/delta_I/delta_I_", (countryname), ".csv"), 1, 0);
        
        dist = csvread(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/dist/dist_", (countryname), ".csv"), 1, 0);
        
        I = csvread(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/I/I_", (countryname), ".csv"), 1, 0);
        productivity = csvread(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/productivities/productivities_", (countryname), ".csv"), 1, 0);

        if countryname == "Nigeria"
           delta_tau = 0.0558/1.03 .* log(dist ./ 1.609);
           delta_tau(dist == 0) = 0;
        else
            delta_tau = 0.0374/0.43 .* log(dist ./ 1.609);
            delta_tau(dist == 0) = 0;
        end

        
        % Basic characteristics of the economy
        J = size(productivity, 1);
        N = size(productivity, 2);

        % Only optimise over the domestic grid cells
        weights = 1-case_centroids.abroad;
        
        K = 1.0;
        
        %% Initialise geography
        
        param = init_parameters('Annealing', 'off', 'LaborMobility', 'off', 'a', a, 'sigma', sigma, 'N', N, 'alpha', alpha, 'beta', beta, 'gamma', gamma, 'verbose', 'off', 'rho', rho, 'K', K);

        [param,g]=create_graph(param,[],[],'X',case_centroids.x,'Y',case_centroids.y,'Type','custom','Adjacency',adj,'X',case_centroids.x,'Y',case_centroids.y);
        
        
        param.Lj = population + (population == 0) .* 10^-7; % this is needed as ipopt crashes otherwise (need to have population > 0 always), so I add an infinitisimal person
        param.Zjn = productivity;
        param.omegaj = weights;
        
        param.Hj = population .* (1-alpha); % I normalise this because the general utility function has a (h_j/(1-alpha))^(1-alpha) thing with it
        
        
        g.delta_i = delta_I;
        g.delta_tau = delta_tau;
       


        %% Optimisation
        
        min_mask = abr .* I + (adj - abr) .* 4;        
        max_mask = abr .* I + (adj - abr) .* 120;
        
        
        % Static
        strcat("Started fixed point on ", datestr(datetime('now')))
        [res_stat,flag,x0] = solve_allocation(param,g,I);

      

        % delete after
        counter = 0;
        converged = false;
        A = ((res_stat.Qjkn .^ beta) ./ (I .^ gamma));
        old_factor = mean(A(res_stat.Qjkn > 0))
        %if num_locations < 80
            while ~converged
            g.delta_tau = delta_tau ./ old_factor;
            [res_stat,flag,x0] = solve_allocation(param,g,I,false,x0);
            A = ((res_stat.Qjkn .^ beta) ./ (I .^ gamma));
            factor = mean(A(res_stat.Qjkn > 0))
            
            counter = counter + 1;
            if counter > 100 || abs(old_factor - factor) < 0.001
                strcat(countryname, ": ", string(factor), ", after ", string(counter))
                converged = true;
            else
                old_factor = factor;
            end
            end
        %end
        cs(end+1) = string(countryname);
        pops(end+1) = sum(res_stat.Lj);
        nums(end+1) = num_locations;
        facts(end+1) = factor;

 
       end
    end
 end

writetable(array2table([cs' pops' nums' facts'], ...
          'VariableNames', {'countryname', 'pop', 'num_loc', 'factor'}), strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/tau_factors_ethnga.csv"));
