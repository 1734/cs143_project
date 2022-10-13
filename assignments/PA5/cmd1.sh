for input_file in $@; do
    file_base_name=$(basename $input_file)
    file_base_name_without_ext="${file_base_name%.*}"
    echo $file_base_name_without_ext
    dir_name=$(dirname $input_file)
    original_output="${dir_name}/${file_base_name_without_ext}.s"
    my_output="${dir_name}/${file_base_name_without_ext}_my.s"
    ./coolc -o "${original_output}" $input_file
    ./mycoolc -o "${my_output}" $input_file
    code --diff $original_output $my_output
done
