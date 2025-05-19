PGM=slice-ipp

SRCS=$(PGM).f $(PGM)-version.inc ipp-lite.f \
     comn/fnbc.f comn/lnbc.f comn/getendian.f comn/julian.f comn/xxrev.f \
     opus-comn/header_indices.inc opus-comn/opus_constants.inc \
     opus-comn/read_input_line.f \
     opus-comn/get_opus_xx.f opus-comn/get_opusigram_params.f \
     opus-comn/build_cit_name.f opus-comn/save_to_file.f \
     ../ipp/parse_input/open_input_file.f \
     ../ipp/parse_input/close_input_file.f \
     ../ipp/parse_input/read_params.f

IDIR=opus-comn
