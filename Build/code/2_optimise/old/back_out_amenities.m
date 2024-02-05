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
sigma = 4.0;
a = 0.7;
rho = 0.0; % this is really important to not have any inequality aversion. I am not entirely sure if thats legit because in their toolbox, FS say rho >= 1... but i see no reason why 0 should not be ok...


for countryID = 1:length(country_names)
    
     countryname = (country_names(countryID))
     if exist(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/productivities/productivities_", (countryname), ".csv"))  && ~exist(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/amenities/am_", (countryname), ".csv"))

        % Split centroids by country
        case_centroids = readtable(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/borderregions/", (countryname), "_borderregion.csv"));
        num_locations = size(case_centroids, 1)
     if num_locations > 2 && num_locations < 100
        %if countryname == "United-States"
        % Read in characteristics
        population = case_centroids.pop;


        % Read in the relevant matrices
        adj = csvread(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/adj/adj_", (countryname), ".csv"), 1, 0);
        abr = csvread(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/abr/abr_", (countryname), ".csv"), 1, 0);
        delta_I = csvread(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/delta_I/delta_I_", (countryname), ".csv"), 1, 0);
        delta_tau = csvread(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/delta_tau/delta_tau_", (countryname), ".csv"), 1, 0);
        I = csvread(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/I/I_", (countryname), ".csv"), 1, 0);
        productivity = csvread(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/productivities/productivities_", (countryname), ".csv"), 1, 0);

        
        % Basic characteristics of the economy
        J = size(productivity, 1);
        N = size(productivity, 2);

        % Only optimise over the domestic grid cells
        weights = 1-case_centroids.abroad;
        
        K = 1.0;
        
        % set it up

        param = init_parameters('Annealing', 'off', 'LaborMobility', 'on', 'a', a, 'sigma', sigma, 'N', N, 'alpha', alpha, 'beta', beta, 'gamma', gamma, 'verbose', 'on', 'rho', rho, 'K', K);
        [param,g]=create_graph(param,[],[],'X',case_centroids.x,'Y',case_centroids.y,'Type','custom','Adjacency',adj,'X',case_centroids.x,'Y',case_centroids.y); %,  'NRegions', 24, 'Region', regions);
                
        param.Lj = population + (population == 0) .* 10^-7; % this is needed as ipopt crashes otherwise (need to have population > 0 always), so I add an infinitisimal person
        param.Zjn = productivity;
        
        pop_target = population ./ sum(population);
        
        param.Hj = ones(J,1);  
        
        
        g.delta_i = delta_I;
        g.delta_tau = delta_tau;
        
        converged = false;
        i = 1;
        
        while ~converged
        
            res_stat3 = solve_allocation(param,g,I, false);
            
            target_adjustment = pop_target ./ res_stat3.Lj;
            
            abs_diff = sum(abs(target_adjustment - 1)) ./ J;
            if abs_diff < 0.0001 || i > 1000
                converged = true;
            end
            
            i = i +1;
            strcat(datestr(datetime('now')), ": ", countryname, " - ", string(i), ": Target deviation: ", string(abs_diff))
            param.Hj = param.Hj.* target_adjustment;
        
        end

        csvwrite(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/amenities/am_", (countryname), ".csv"), param.Hj);


     end
     end
end