function [I_opt_sym] = I_opt_sym(P, I, adj, abr, delta_tau, delta_I, beta, gamma)
%UNTITLED2 Summary of this function goes here
%   Detailed explanation goes here

kappa = gamma*(1+beta)^(-(1+beta)/beta);

J = size(P, 1);
N = size(P, 2);


P_transform = reshape(P, [J, 1, N]);

for n=1:N
   P_n = P_transform(:,:,n);
   
   inner_bracket_sliced_1(:,:,n) = (max(zeros(J), transpose(P_n * transpose(P_n.^-1)) -1).^((1+beta)/beta)) .* P_n .* (delta_tau).^(-1/beta);
   inner_bracket_sliced_2(:,:,n) = (max(zeros(J), (P_n .* transpose(P_n.^-1)) -1).^((1+beta)/beta)) .* transpose(P_n) .* transpose((delta_tau).^(-1/beta));

end


inner_bracket = (sum(inner_bracket_sliced_1, 3) + sum(inner_bracket_sliced_2, 3)) .* adj;


% Obtain the multiplier

% mu = sum(sum((((kappa .* inner_bracket .* ((delta_tau).^(-1/beta))).^(beta/(gamma-beta))) .* (delta_I.^(gamma/(beta-gamma)))), 1, 'omitnan'), 2, 'omitnan').^((gamma-beta)/beta);


% Obtain I_opt

I_raw = ((kappa .* (delta_I + transpose(delta_I)).^(-1)) .* inner_bracket).^(beta/(beta-gamma));
I_raw(isnan(I_raw)) = 0;

% mu1 = 1/sum(sum(I_raw .* delta_I));

I_opt_sym_nofour = I_raw * 1/sum(sum(I_raw .* delta_I)); % this denominator is just = 1 if I_raw were already perfect

% Idea for preserving fours:

interimmatrix = max(zeros(J), (I_opt_sym_nofour - 4)); % I only consider cells above walking speed

mu2 = (1 - sum(sum(ones(J) .* 4 .* (adj-abr) .* delta_I))) / sum(sum(interimmatrix .* delta_I .* (adj-abr))); % I rescale those cells by the available infrastructure (which is everything above walking speed)

I_opt_sym_bbelow = (interimmatrix * mu2) + (4 .* adj); % I add the walking speed to those that were computed to have below walking speed


% Replace the abroad ones:
I_opt_sym_replaceabroad = I_opt_sym_bbelow * 1/(sum(sum(I_opt_sym_bbelow .* (adj - abr) .* delta_I)));
I_opt_sym_replaceabroad = I_opt_sym_replaceabroad .* (adj - abr) + I .* abr;


%I_opt_sym = min((ones(J) .* 100 .* adj), I_opt_sym_replaceabroad);
I_opt_sym = I_opt_sym_replaceabroad;

end

