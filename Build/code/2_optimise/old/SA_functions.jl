using CSV: length
using Base: full_va_len # full_warning_showed
using Missings: length

using DataFrames #, Missings
using CSV
using DataFramesMeta
using JSON
using DelimitedFiles
# using Statistics
using StatsBase
# using FreqTables
using Random
using LinearAlgebra

# profiling:
using BenchmarkTools
using Profile
using TimerOutputs

function read_environment(country)
    basepath = "/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/"

    
    abr = readdlm(string(basepath, "abr/abr_", country, ".csv"), ',', Float64, header = true)[1]::Matrix{Float64}
    adj = readdlm(string(basepath, "adj/adj_", country, ".csv"), ',', Float64, header = true)[1]::Matrix{Float64}
    delta_I = readdlm(string(basepath, "delta_I/delta_I_", country, ".csv"), ',', Float64, header = true)[1]::Matrix{Float64}
    delta_tau = readdlm(string(basepath, "delta_tau/delta_tau_", country, ".csv"), ',', Float64, header = true)[1]::Matrix{Float64}
    I = readdlm(string(basepath, "I/I_", country, ".csv"), ',', Float64, header = true)[1]::Matrix{Float64}
    productivity = readdlm(string(basepath, "productivities/productivities_", country, ".csv"), ',', Float64, header = true)[1]::Matrix{Float64}






end