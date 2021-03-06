-module(job_centre2).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([add_job/1, work_wanted/0, job_done/1, statistics/0]).

-define(SERVER, ?MODULE).

-record(state, {job_number=0, jobs=orddict:new(), in_progress=orddict:new(), completed=orddict:new()}).


add_job(F) ->
    gen_server:call(?MODULE, {add, F}).
work_wanted() ->
    { work, Reply } = gen_server:call(?MODULE, {work_request}),
    case Reply of
        { JobNumber, F } ->
            Result = F(),
            io:format("Work result = ~p.~n", [Result]),
            job_done(JobNumber);
        no -> done
    end.
job_done(JobNumber) ->
    gen_server:call(?MODULE, {job_done, JobNumber}).
statistics() ->
    { statistics, Stats } = gen_server:call(?MODULE, { statistics }),
    io:format("Stats: ~p.~n", [Stats]).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call({ add, F }, _From, #state{job_number=Num, jobs=Queue, in_progress=InProgress, completed=Completed}) ->
    {reply, {job_added, Num+1}, #state{job_number=Num+1, jobs=orddict:store(Num+1, F, Queue), in_progress=InProgress, completed=Completed}};
handle_call({ job_done, JobNumber }, _From, #state{job_number=Num, jobs=Queue, in_progress=InProgress, completed=Completed}) ->
    io:format("Job ~p has been completed.~n", [JobNumber]),
    { ok, Job } = orddict:find(JobNumber, InProgress),
    {reply, ok, #state{job_number=Num, jobs=Queue, in_progress=orddict:erase(JobNumber, InProgress), completed=orddict:store(JobNumber, Job, Completed)}};
handle_call({ work_request }, _From, #state{job_number=Num, jobs=Queue, in_progress=InProgress, completed=Completed}) ->
    case orddict:size(Queue) > 0 of
        true -> { Key, Val } = hd(orddict:to_list(Queue)),
                {reply, { work, { Key, Val } }, #state{job_number=Num, jobs=orddict:erase(Key, Queue), in_progress=orddict:store(Key, Val, InProgress), completed=Completed}};
        false ->
            {reply, { work, no }, #state{job_number=Num, jobs=Queue, in_progress=InProgress, completed=Completed}}
    end;
handle_call({ statistics }, _From, #state{job_number=Num, jobs=Queue, in_progress=InProgress, completed=Completed}) ->
    {reply, { statistics, {Num, Queue, InProgress, Completed} }, #state{job_number=Num, jobs=Queue, in_progress=InProgress, completed=Completed}};
handle_call(Request, _From, State) ->
    io:format("Unknow Message: ~p.~n", [Request]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
