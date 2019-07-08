if [ $# -gt 0 ] ; 
then
    IN=$1
    if [ $# -gt 1 ] ; 
    then
        OUT=$2
    else 
    file=${IN%.*}
    OUT=${file/\//_}.svg
    fi
else
    IN=src/main.rs
    OUT=output1.svg
fi

RUST_LOG=hello_world=trace cargo run ${IN} 2>debug.log
echo "input:  ${IN}" 
echo "dot:    output.dot"
echo "debug:  debug.log"

if ! [ -x "$(command -v dot)" ];
then
    echo "You should install Graphviz(https://www.graphviz.org/) first if you want to Visualize the \'.dot\' file"
    exit 2
fi

dot -Tsvg output.dot -o ${OUT}
echo "output: ${OUT}"
