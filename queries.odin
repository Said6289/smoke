// By Morten Vassvik

package main

import gl "shared:odin-gl";


Query_Type :: enum {
    Initialization_Velocity,
    Initialization_Pressure,
    Initial_Divergence,
    Initial_Residual,
    Pressure,
    Gradient,
    Final_Divergence,
    Final_Residual,
    Display,
}

NUM_QUERIES :: len(Query_Type);
NUM_QUERY_SLOTS :: 32;
query_objects: [NUM_QUERIES][NUM_QUERY_SLOTS]u32;
queries_elapsed: [NUM_QUERIES][NUM_QUERY_SLOTS]u64;    // Store the timestamps, in nanoseconds
queries_activated: [NUM_QUERIES][NUM_QUERY_SLOTS]bool; // To keep track of which query objects are in-flight
query_index := 0;

@(deferred_in=end_query_block)
query_block :: proc(query_type: Query_Type) {
    // WARNING: Cannot be nested
    // WARNING: Can only be used once per frame
    gl.BeginQuery(gl.TIME_ELAPSED, query_objects[query_type][query_index % NUM_QUERY_SLOTS]);
    queries_activated[query_type][query_index % NUM_QUERY_SLOTS] = true;
}

end_query_block :: proc(query_type: Query_Type) {
    // WARNING: Do not call this explicitly. 
    gl.EndQuery(gl.TIME_ELAPSED);
}

init_queries :: proc() {
    // WARNING: Must be called once before the main loop
    gl.GenQueries(i32(NUM_QUERIES*NUM_QUERY_SLOTS), &query_objects[0][0]);
}

check_queries :: proc() {
    // Checks to see if any queries of the activated queries are completed, in which case get the results
    // WARNING: Must be called once per frame
    for _, i in queries_activated {
        for _, j in queries_activated[i] {
            if !queries_activated[i][j] {
                continue;
            }
            ready: u64;
            gl.GetQueryObjectui64v(query_objects[i][j], gl.QUERY_RESULT_AVAILABLE, &ready);
            if ready == gl.TRUE {
                gl.GetQueryObjectui64v(query_objects[i][j], gl.QUERY_RESULT_NO_WAIT, &queries_elapsed[i][j]);
                queries_activated[i][j] = false;
            }
        }
    }

    query_index += 1;
}

get_query_averages :: proc(query_type: Query_Type, mem_size: int) -> (time, bandwidth: f64) {
    avg_time := f64(0.0);
    for time in queries_elapsed[query_type] {
        avg_time += f64(time);
    }
    avg_time /= f64(len(queries_elapsed[query_type]));
    avg_time *= 1.0e-9;

    return avg_time * 1000.0, f64(mem_size) / f64(1024 * 1024 * 1024) / avg_time; // time in milliseconds, bandwidth in GiB/s
}
