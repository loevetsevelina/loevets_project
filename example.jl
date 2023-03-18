using Dates
using BlackBoxOptim
#M=14                    #кол во студентов
T=30                    #период планирования 
N=8                    #кол во заданий
eps=0.0001              #эпсилон
iterations=10           #кол во итераций
neighbours=10           #кол во рассматриваемых соседей
MAX = 50

struct Student
    p::Float64
    q::Float64
    a::Float64
    b::Float64
    l
end

struct  dates
    id::Int
    start::Int
    finish::Int
end

struct Assignment
    id::Int
    B::Float64
end

function generate_student()
    p=rand(0.0:0.05:0.95)
    q=rand(0.75:0.05:0.95)
    a=rand(2:0.05:4)
    b=rand(1:5)
    l=zeros(Int,1,7)
    for i in 1:7
        l[i]=rand(0:5)
    end
    return Student(p,q,a,b,l)
end

function generate_assignment(id)
    B=rand(30:60)
    return Assignment(id,B)
end

function generate_date(id)
    start=rand(0:T-1)
    finish=rand(start+1:T)
    return dates(id,start,finish)
end

function find_active(t,tasks_done,d)
    res=Array{Int}(undef,0)
    for i in 1:N
        if d[i].start<=t && d[i].finish>t && !(i in tasks_done)
            push!(res,i)
        end
    end
    return res
end

function u(t,s)          
    return s.a*(s.l[t%7+1])+s.b
end

function G(id,t,active,s,a,d)
    if !(id in active)
        return 0
    end
    k=d[id].finish-t
    return Gk(t,k,id,active,s,a,d)
end

function Gk(t,k,id,active,s,a,d)
    tmp_active=Array{Int}(undef,0)
    for i in active
        if d[i].start<=t && d[i].finish>t
            push!(tmp_active,i)
        end
    end
    if k==1
        return max(0.0, a[id]*s.q-u(t,s))
    end
    return max(a[id]*s.q^k-Gk(t+1,k-1,id,active,s,a,d)*s.q*(1-s.p)-u(t,s),0.0)
end

function find_avaible(t,d)
    res=Array{Int}(undef,0)
    for i in 1:N
        if d[i].start<=t && d[i].finish>t
            push!(res,i)
        end
    end
    return res
end

function cost_vector(t,active,s,a,d)
    g=[]
    for i in 1:N
        push!(g,G(i,t,active,s,a,d))
    end
    return g
end

tasks_res=zeros(Float64,N)
day_of_new_tree=[]

function new_day(t,value,tasks_done,s,a,d,k_i,points)
    if t>=T 
        push!(Points,(points,value))
        return 
    end
    if points>=MAX
        push!(Points,(points,value))
        return
    end
    avaible=find_avaible(t,d)
    if length(avaible)==0
        if !(t in day_of_new_tree)
            push!(day_of_new_tree,t)
            new_day(t+1,1,tasks_done,s,a,d,k_i,points)
        end
        return
    end
    active=find_active(t,tasks_done,d)
    if length(active)==0
        new_day(t+1,value,tasks_done,s,a,d,k_i,points)
        return
    end
    g=cost_vector(t,active,s,a,d)
    max_id=findall(x->x==maximum(g),g)[1]
    if g[max_id]==0
        new_day(t+1,value,tasks_done,s,a,d,k_i,points)
        return
    end
    if value<eps
        return 
    end
    deleteat!(active,findall(x->x==max_id,active))
    k_i[max_id] += 1
    tasks_res[max_id]+=value*(1-s.p)
    g[max_id]=0
    new_day(t+1,value*s.p,tasks_done,s,a,d,k_i,points)
    new_day(t+1,value*(1-s.p),union(tasks_done,max_id),s,a,d,k_i,points+a[max_id]) ##заменить на new_day(t+1,value*s.(1-0)p,union(tasks_done,max_id),s,a,d,student_indx)
    return
end

#asig=Array{Assignment}(undef,N)
assig = Array{Float64,1}(undef,N)
d=Array{dates}(undef,N)
for i in 1:N
    #asig[i]=generate_assignment(i)
    assig[i] = rand(30:60)
    d[i]=generate_date(i)
end

K_i = []
Points=[]

function ExpectedValue(K,Eval,s,a)
    assigment=zeros(Float64,M)
    for i in 1:M
        st=s[i]
        k_i=K[i]
        E=zeros(Float64,N)
        for j in 1:N
            E[j] = 1 - (st.p)^k_i[j]
            assigment[i] += E[j]*a[j].B
        end
        push!(Eval,E)

    end
    return assigment
end

function counter(a)
    d=Array{dates}(undef,N)
    for i in 1:N
        d[i]=generate_date(i)
    end
    student = generate_student()
    global tasks_res,day_of_new_tree
    tasks_res=zeros(Float64,N)
    day_of_new_tree=[]
    points=0
    k_i = zeros(N)              # массив для i  студента с ki for bi
    new_day(0,1,[],student,a,d,k_i,points)
    #=for j in 1:N
        print(k_i[j], "  ")
    end=#
    E=zeros(Float64,N)
    Etask=0
    for j in 1:N
        E[j] = 1 - (student.p)^k_i[j]
        Etask += E[j]
    end
    return Etask
end

for i in 1:N
    print(assig[i]," ")
end

print(counter(assig))


#FitnessScheme=ParetoFitnessScheme{1}(is_minimizing=false)
#=res1 = bboptimize(counter; SearchRange = (30, 60),  NumDimensions = N, Method =:random_search, MaxTime = 5.0) 
bs = best_candidate(res1)
bf = best_fitness(res1)
println(bf)=#


