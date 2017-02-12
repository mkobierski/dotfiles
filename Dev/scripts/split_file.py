def is_section_end(line):
    return line == "\n"

def write_section(first_line, in_file, out_file):
    out_file.write(first_line)
    lines_written = 1
    line = in_file.readline()
    while line and not is_section_end(line):
        out_file.write(line)
        lines_written += 1
        line = in_file.readline()
    return lines_written, line

def split_file(fname='TAGS'):
    section_id = 0
    max_lines = 200000
    with open(fname, 'r') as tags:
        line = tags.readline()
        while line:
            linecount = 0
            with open(fname + str(section_id), 'w') as out:
                while line and linecount < max_lines:
                    lines_written, line = write_section(line, tags, out)
                    linecount += lines_written
            section_id += 1
