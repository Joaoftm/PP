import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;


public class Main {
	/*
	*@param String[] args - args passados ao Main na linha de comandos
	*@requires - São esperados 3 parametros no String args.
	*			 O path para o ficheiro metrics.txt(ou nome alternativo), que deverá conter metricas para 
	*			 inicialização de agregadores no processWrapper. O Path para o ficheiro de dados dataset.csv(ou nome alternativo),
	*			 que deverá conter as linhas de input a passar ao STDIN através do método WriteLine do ProcessWrapper. O path do ficheiro 
	*			 para execução do Agregador, que será usado para inicializar os vários ProcessWrappers, para cada métrica.
	*/
	public static void main(String[] args) {
		
		String metricsPath = args[0];
		
		String dataPath = args[1];
		
		String aggPath = args[2];

		try {
			Stream<String> metricStream = Files.lines(Paths.get(metricsPath));
			Stream<String> dataStream = Files.lines(Paths.get(dataPath));
			Stream.Builder<ProcessWrapper> pwrapB = Stream.builder();
			dataStream = dataStream.map(line -> line.replace(',', ' '));

			metricStream.forEach(entry -> {
					ProcessWrapper temp = new ProcessWrapper(aggPath);
					temp.writeLine(entry);
					pwrapB.add(temp);
				}
			);
			ProcessWrapper[] pwrap = pwrapB.build().toArray(ProcessWrapper[]::new);

			dataStream.forEach(dline -> {
					String linha = "";
					for(ProcessWrapper t : pwrap){
						t.writeLine(dline);
						linha = linha + t.readLine() + ",";
					}
					linha = linha.substring(0,linha.length()-1);
					System.out.println(linha);
					linha = "";
				}
			);

			for(ProcessWrapper t : pwrap){
				t.kill();
			}

		}
		catch (IOException ex){
			System.out.println(ex.getMessage());
		}

	}
	
}
