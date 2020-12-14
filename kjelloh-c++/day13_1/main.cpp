//  Watch live coding of this solution on youtube https://youtu.be/12hCpj1IucM
//  main.cpp
//  AdventOfCode201213_1
//
//  Created by Kjell-Olov Högdal on 2020-12-14.
//

#include <iostream>
#include <sstream>
#include <vector>
#include <string>
#include <algorithm>
#include <limits>

extern char const* pExampleData;
extern char const* pData;

using TimeStamp = unsigned int;
using Result = unsigned int;
using BusId = TimeStamp;
struct Bus {BusId id; bool in_operation;};
using BusSchedule = std::vector<Bus>;

struct NextBus {
    Bus bus;
    TimeStamp time_until_departure;
};

TimeStamp time_until_departure(TimeStamp const& arrive_time,BusSchedule const& bus_schedule,BusId bus_id) {
    return bus_id - (arrive_time % bus_id);
}

NextBus next_bus_at(TimeStamp const& arrive_time,BusSchedule const& bus_schedule) {
    // Time for some modulo calculation :)
    // Time since departure for a bus is arrive_time modulo Bus_id :)
    // Thus time to next departure is Bus_id - time_since_departure
    auto next_bus_iter = std::min_element(bus_schedule.begin(), bus_schedule.end(), [&bus_schedule,&arrive_time](auto const& bus1,auto const& bus2){
        return time_until_departure(arrive_time, bus_schedule, bus1.id) < time_until_departure(arrive_time, bus_schedule, bus2.id);
    });
    return {*next_bus_iter,time_until_departure(arrive_time, bus_schedule, next_bus_iter->id)};
}

int main(int argc, const char * argv[]) {
    // In
    TimeStamp arrive_time;
    BusSchedule bus_schedule;
    // std::basic_istringstream<char> in {pExampleData}; // test
    std::basic_istringstream<char> in {pData};
    in >> arrive_time;
    std::string sBusList;
    in >> sBusList;
    std::basic_istringstream<char> busses{sBusList};
    std::string sBusId;
    while (std::getline(busses, sBusId, ',')) {
        std::cout << "\nBusId:" << sBusId;
        if (sBusId == "x") {
            bus_schedule.push_back({std::numeric_limits<TimeStamp>::max(),false});
        }
        else {
            TimeStamp bus_id = std::stoi(sBusId);
            bus_schedule.push_back({bus_id,true});
        }
    }

    // Out
    NextBus next_bus = next_bus_at(arrive_time,bus_schedule);
    Result result = next_bus.bus.id * next_bus.time_until_departure;
    
    std::cout << "\nResult = " << "bus_id:" << next_bus.bus.id << " * wait time " << next_bus.time_until_departure
    << " = " << result;
    
    std::cout << "\n\n";
    return 0;
}

char const* pExampleData = R"(939
7,13,x,x,59,x,31,19)";

char const* pData = R"(1000303
41,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,541,x,x,x,x,x,x,x,23,x,x,x,x,13,x,x,x,17,x,x,x,x,x,x,x,x,x,x,x,29,x,983,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19)";
