from obspy import UTCDateTime
from hyposatpy import locate, HyposatPick, HyposatStation, HyposatFlags


if __name__ == "__main__":

    # define stations
    stations = [
        HyposatStation(
            name="FINES",
            latitude=61.4436,
            longitude=26.0771,
            elevation=0.138
        ),
        HyposatStation(
            name="SPITS",
            latitude=78.1777,
            longitude=16.37,
            elevation=0.323
        )
    ]

    # define picks
    picks = [
        HyposatPick(
            station="FINES",
            phase="Sn",
            time=UTCDateTime(2020, 1, 2, 6, 1, 43, 800000),
            time_error=2.0,
            back_azimuth=35.2,
            back_azimuth_error=20.0,
            slowness=24.17,
            slowness_error=2.5,
            flags=HyposatFlags("TASD_M_"),
            period=0.142,
            amplitude=0.96,
            snr=10.8,
            fkx_num=0
        ),
        HyposatPick(
            station="SPITS",
            phase="Pn",
            time=UTCDateTime(2020, 1, 2, 6, 0, 17, 0),
            time_error=1.0,
            back_azimuth=136.8,
            back_azimuth_error=20.0,
            slowness=19.51,
            slowness_error=2.5,
            flags=HyposatFlags("TASD_M_"),
            period=0.231,
            amplitude=0.12,
            snr=19.2,
            fkx_num=1
        ),
        HyposatPick(
            station="SPITS",
            phase="Sn",
            time=UTCDateTime(2020, 1, 2, 6, 2, 13, 200000),
            time_error=2.0,
            back_azimuth=133.7,
            back_azimuth_error=20.0,
            slowness=38.34,
            slowness_error=2.5,
            flags=HyposatFlags("TASD_M_"),
            period=0.394,
            amplitude=0.3,
            snr=6.2,
            fkx_num=2
        ),
    ]

    # run location
    output, parameters = locate(picks, stations)

    num_origins = len(output.origins)
    print("got {} origin(s)!".format(num_origins))

    # Print out basic info about the event (example only)
    if num_origins > 0:
        origin = output.origins[-1]
        print("Time:       {}".format(origin.time))
        print("Latitude:   {}°".format(origin.latitude))
        print("Longitude:  {}°".format(origin.longitude))
        print("Depth:      {} km".format(origin.depth))
        print("Region:     {}({})".format(*origin.flinn_engdalh_region))
        print("# Arrivals: {}".format(len(origin.arrivals)))
