
% This file takes adj, dist, and speed matrices and calculates current tradeflows, optiomal networks, and welfare gains

%% Read in global data and define parameters

clear;

country_table = readtable("/Users/tilman/Documents/GitHub/Thesis_Git/Build/temp/country_names.csv");
country_names = table2array(country_table);

centroids = readtable("/Users/tilman/Documents/GitHub/Thesis_Git/Build/temp/centroids.csv", 'TreatAsEmpty',{'.','NA'}, 'EmptyValue', 0);
centroids.country = categorical(centroids.country);

% Defines parameters

alpha = 0.4;
beta = 1.245;
gamma = 0.5*beta;
%sigma = 4;
sigma = 1.5;
a = 0.7;
% kappa = gamma*(1+beta)^(-(1+beta)/beta);

% Defines FOCs (together with more complex functions Q and I)
c = @(P) ((1/alpha) * (sum(P.^(1-sigma), 2)).^(1/(1-sigma))).^(1/(alpha-1)); % per capita total consumption in each location
C = @(P, population) (P./(sum(P.^(1-sigma), 2)).^(1/(1-sigma))).^(-sigma) .* (population.*c(P));
L = @(P, productivity, population) (((P .* productivity) .^(1/(1-alpha))) ./ sum((P .* productivity) .^(1/(1-alpha)), 2)) .* population;

outflows = @(P, I, delta_tau, delta_I, adj) reshape(sum(Q(P, I, adj, delta_tau, beta, gamma) + delta_tau .* ((Q(P, I, adj, delta_tau, beta, gamma)).^(1+beta)) .* I.^(-gamma), 2, 'omitnan'), [size(P,1), size(P,2)]);
inflows = @(P, I, delta_tau, delta_I, adj) reshape(sum(Q(P, I, adj, delta_tau, beta, gamma), 1), [size(P,1), size(P,2)]);


%% For each country

 %for countryID = 1:length(country_names)
 for countryID = 6
    countryname = (country_names(countryID))
     if exist(strcat("/Users/tilman/Documents/GitHub/Thesis_Git/Build/temp/productivities/productivities_", (countryname), ".csv"))  && ~exist(strcat("//Users/tilman/Documents/GitHub/Thesis_Git/Build/output/Network_outcomes/", (countryname), "_outcomes.csv"))

        % Split centroids by country
        case_centroids = readtable(strcat("/Users/tilman/Documents/GitHub/Thesis_Git/Build/temp/borderregions/", (countryname), "_borderregion.csv"));
        num_locations = size(case_centroids, 1)
     if num_locations > 2 && num_locations < 200

        % Read in characteristics
        population = case_centroids.pop;


        % Read in the relevant matrices
        adj = csvread(strcat("/Users/tilman/Documents/GitHub/Thesis_Git/Build/temp/adj/adj_", (countryname), ".csv"), 1, 0);
        abr = csvread(strcat("/Users/tilman/Documents/GitHub/Thesis_Git/Build/temp/abr/abr_", (countryname), ".csv"), 1, 0);
        delta_I = csvread(strcat("/Users/tilman/Documents/GitHub/Thesis_Git/Build/temp/delta_I/delta_I_", (countryname), ".csv"), 1, 0);
        delta_tau = csvread(strcat("/Users/tilman/Documents/GitHub/Thesis_Git/Build/temp/delta_tau/delta_tau_", (countryname), ".csv"), 1, 0);
        I = csvread(strcat("/Users/tilman/Documents/GitHub/Thesis_Git/Build/temp/I/I_", (countryname), ".csv"), 1, 0);
        productivity = csvread(strcat("/Users/tilman/Documents/GitHub/Thesis_Git/Build/temp/productivities/productivities_", (countryname), ".csv"), 1, 0);

        
        % Basic characteristics of the economy
        J = size(productivity, 1);
        N = size(productivity, 2);

        % Dont really need them now, as they are all ones. But might come
        % in handy later?
        weights = 1-case_centroids.abroad;

        %% Optimisation

        %{ defines static problem with current infrastructure
        Lagrange_I_static  = @(P) deal((sum(weights .* population .* (c(P).^(alpha)), 1) ...
        - sum(sum(P .* (C(P, population) + outflows(P, I, delta_tau, delta_I, adj) - inflows(P, I, delta_tau, delta_I, adj) ...
        - productivity .* (population.^(a)))))), ...
        -reshape(C(P, population) + outflows(P, I, delta_tau, delta_I, adj) - inflows(P, I, delta_tau, delta_I, adj) ...
        - productivity .* (population.^(a)), [J*N 1]));

        % defines full problem with optimal infrastructure
        Lagrange_I_opt  = @(P) deal((sum(weights .* population .* (c(P).^(alpha)), 1) ...
        - sum(sum(P .* (C(P, population) + outflows(P, I_opt_sym(P, I, adj, abr, delta_tau, delta_I, beta, gamma), delta_tau, delta_I, adj) - inflows(P, I_opt_sym(P, I, adj, abr, delta_tau, delta_I, beta, gamma), delta_tau, delta_I, adj) ...
        - productivity .* (population.^(a)))))), ...
        -reshape(C(P, population) + outflows(P, I_opt_sym(P, I, adj, abr, delta_tau, delta_I, beta, gamma), delta_tau, delta_I, adj) - inflows(P, I_opt_sym(P, I, adj, abr, delta_tau, delta_I, beta, gamma), delta_tau, delta_I, adj) ...
        - productivity .* (population.^(a)), [J*N 1]));
        %}
        
        Lagrange_I_static  = @(P) (sum(weights .* population .* (c(P).^(alpha)), 1) ...
        - sum(sum(P .* (C(P, population) + outflows(P, I, delta_tau, delta_I, adj) - inflows(P, I, delta_tau, delta_I, adj) ...
        - productivity .* (population.^(a))))));
    
        Lagrange_I_opt  = @(P) sum(weights .* population .* (c(P).^(alpha)), 1) ...
        - sum(sum(P .* (C(P, population) + outflows(P, I_opt_sym(P, I, adj, abr, delta_tau, delta_I, beta, gamma), delta_tau, delta_I, adj) - inflows(P, I_opt_sym(P, I, adj, abr, delta_tau, delta_I, beta, gamma), delta_tau, delta_I, adj) ...
        - productivity .* (population.^(a)))));
        
        % Define options for different solving algorithms (On 10. Dec, I
        % prefer trustregion)
        %options_interiorpoint = optimoptions('fmincon','SpecifyObjectiveGradient',true, 'MaxIterations', 5000, 'MaxFunctionEvaluations', 30000);
        %options_trustregion = optimoptions('fmincon','SpecifyObjectiveGradient',true, 'Algorithm', 'trust-region-reflective');
        options_interiorpoint = optimoptions('fmincon','SpecifyObjectiveGradient',false, 'MaxIterations', 50000, 'MaxFunctionEvaluations', 300000, 'Algorithm', 'sqp');

        % Set initial conditions and solver boundaries
        P0 = rand(J, N).*100;
        % [~, ~, ranking] = unique(sum(productivity,2));
        % P0 = [ranking ranking] + rand(J, N); % This makes the starting value proportional to the rank of productivity of a given location, hoping that this speeds up computation time.
        A = [];
        b = [];
        Aeq = [];
        beq = [];
        lb = zeros(J, N);
        ub = [];
        nonlcon = [];

        % Either take P_stat from Online
        % if exist(strcat("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/MatlabOnline_results/P_stat", (countryname), ".csv"))
        %     P_stat = csvread(strcat("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/MatlabOnline_results/P_stat", (countryname), ".csv"));
        %     datetime('now') % just for display purposes
        % else
            % Optimise static problem
        strcat("Started P_stat on ", datestr(datetime('now')))
        [P_stat] = fmincon(Lagrange_I_static, P0,A,b,Aeq,beq,lb,ub, nonlcon, options_interiorpoint);
        csvwrite(strcat("/Users/tilman/Documents/GitHub/Thesis_Git/Build/output/Prices/P_stat/P_stat_", (countryname), ".csv"), P_stat);
        
        strcat("Finished P_stat on ", datestr(datetime('now'))) % just for display purposes
        % end

        
        %mycons = @(P) (constraint_function(P, I, adj, abr, delta_tau, delta_I, beta, gamma));

        
        
        % Optimise full problem
        strcat("Started P_opt on ", datestr(datetime('now'))) % just for display purposes
        [P_opt] = fmincon(Lagrange_I_opt, P_stat,A,b,Aeq,beq,lb,ub, nonlcon, options_interiorpoint);
        csvwrite(strcat("/Users/tilman/Documents/GitHub/Thesis_Git/Build/output/Prices/P_opt/P_opt_", (countryname), ".csv"), P_opt);

        strcat("Finished P_opt on ", datestr(datetime('now'))) % just for display purposes

        %% Obtain descriptive statistics

        optimal_infrastructure = I_opt_sym(P_opt, I, adj, abr, delta_tau, delta_I, beta, gamma);

        raw_tradeflows_stat = sum(Q(P_stat, I, adj, delta_tau, beta, gamma), 3);
        raw_tradeflows_opt = sum(Q(P_opt, optimal_infrastructure, adj, delta_tau, beta, gamma),3);

        total_tradeflows_stat = sum(sum(sum(Q(P_stat, I, adj, delta_tau, beta, gamma))));
        total_tradeflows_opt = sum(sum(sum(Q(P_opt, optimal_infrastructure, adj, delta_tau, beta, gamma))));

        consumption_stat = (c(P_stat).^alpha);
        consumption_opt = (c(P_opt).^alpha);

        welfare_stat = sum(sum((c(P_stat).^alpha).*population));
        welfare_opt = sum(sum((c(P_opt).^alpha).*population));

        price_index_stat = (sum(P_stat.^(1-sigma), 2)).^(1/(1-sigma));
        price_index_opt = (sum(P_opt.^(1-sigma), 2)).^(1/(1-sigma));


        %% Plot graphs

        % graph_stat = digraph(raw_tradeflows_stat);
        graph_stat = graph(I);
        edgeweights_stat = (graph_stat.Edges.Weight / (max(graph_stat.Edges.Weight)));
        msize_stat = 25*((c(P_stat).^alpha).*population) / max((c(P_stat).^alpha).*population)+0.1;

        %graph_opt = digraph(raw_tradeflows_opt);
        graph_opt = graph(optimal_infrastructure);
        edgeweights_opt = (graph_opt.Edges.Weight / (max(graph_stat.Edges.Weight)));
        msize_opt = 25*((c(P_opt).^alpha).*population) / max((c(P_opt).^alpha).*population)+0.1;

        win_loss = ((c(P_opt).^alpha).*population) ./ ((c(P_stat).^alpha).*population);

        f = figure;
        p = uipanel('Parent',f,'BorderType','none', 'BackgroundColor','white');
        p.Title = countryname;
        p.TitlePosition = 'centertop';
        p.FontSize = 12;
        p.FontWeight = 'bold';


        subplot(2,1,1, 'Parent', p)
        plot_1 = plot(graph_stat, 'XData', case_centroids.x, 'YData', case_centroids.y, 'Linewidth', edgeweights_stat, 'MarkerSize', msize_stat, 'NodeCData', price_index_stat);
        colorbar;
        title('Current Infrastructure');

        subplot(2,1,2,'Parent', p)
        plot_2 = plot(graph_opt, 'XData', case_centroids.x, 'YData', case_centroids.y, 'Linewidth', edgeweights_opt, 'MarkerSize', msize_opt, 'NodeCData', win_loss);
        colorbar;
        title('Optimal Infrastructure');

        f.PaperPositionMode = 'auto';
        print('-fillpage',strcat("//Users/tilman/Documents/GitHub/Thesis_Git/Build/output/Matlab_graphs/optimised_network/", (countryname), "_graph"),'-dpdf')


        %% Export data

        % Optimal Network
        csvwrite(strcat("/Users/tilman/Documents/GitHub/Thesis_Git/Build/output/Optimised_Networks/", (countryname), ".csv"), optimal_infrastructure);

        % Raw tradeflows and Optimal Tradeflows
        csvwrite(strcat("/Users/tilman/Documents/GitHub/Thesis_Git/Build/output/Tradeflows/Initial_Flows_", (countryname), ".csv"), raw_tradeflows_stat);
        csvwrite(strcat("/Users/tilman/Documents/GitHub/Thesis_Git/Build/output/Tradeflows/Optimal_Flows_", (countryname), ".csv"), raw_tradeflows_opt);


        % Location Characteristics
        writetable(array2table([case_centroids.ID case_centroids.x case_centroids.y case_centroids.abroad population price_index_stat price_index_opt consumption_stat consumption_opt ...
          sum(inflows(P_stat, I, delta_tau, delta_I, adj), 2) sum(outflows(P_stat, I, delta_tau, delta_I, adj), 2) sum(inflows(P_opt, optimal_infrastructure, delta_tau, delta_I, adj), 2) ...
          sum(outflows(P_opt, optimal_infrastructure, delta_tau, delta_I, adj), 2)], ...
          'VariableNames', {'ID', 'x', 'y', 'abroad', 'pop', 'P_stat', 'P_opt', 'util_stat', 'util_opt', 'inflows_stat', 'outflows_stat', 'inflows_opt', 'outflows_opt'}), strcat("/Users/tilman/Documents/GitHub/Thesis_Git/Build/output/Network_outcomes/", (countryname), "_outcomes.csv"));

       end
    end





    clear msize_opt msize_stat plot_1 plot_2 edgeweights_opt edgeweights_stat
end
