package nlp;

import java.util.List;
import java.util.Properties;

import edu.stanford.nlp.ling.CoreAnnotations;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.CoreDocument;
import edu.stanford.nlp.pipeline.CoreSentence;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;

public class TextProcessing {
//count words
	public static int getCount(String text) {
		
		Properties props = new Properties();
		props.setProperty("annotators", "tokenize, ssplit,pos,lemma");
	     StanfordCoreNLP pipeline = new StanfordCoreNLP(props);
	     CoreDocument coreDocument = new CoreDocument(text);
	     pipeline.annotate(coreDocument);
	     int numOfWords= coreDocument.tokens().size();
	   
	   return numOfWords ;
	}
	//count sentences
	public static int getCountSentence(String userInput) {
		Properties props = new Properties();
		props.setProperty("annotators", "tokenize, ssplit,pos,lemma");
	     StanfordCoreNLP pipeline = new StanfordCoreNLP(props);
	     CoreDocument coreDocument = new CoreDocument(userInput);

	     pipeline.annotate(coreDocument);

	     List<CoreSentence> sentences = coreDocument.sentences();
	int count=0;
	     for(CoreSentence sentence : sentences) {
	    	 count++;
	         System.out.println(sentence.toString());
	     }
	     System.out.println(count);
	
	 return count;
	}
	// count parts of speech
	public static int getCountPoS(String userInput) {
		Properties props = new Properties();
		props.setProperty("annotators", "tokenize, ssplit,pos,lemma");
	     StanfordCoreNLP pipeline = new StanfordCoreNLP(props);
	     CoreDocument coreDocument = new CoreDocument(userInput);

	     pipeline.annotate(coreDocument);

	     List<CoreLabel> coreLabelList = coreDocument.tokens();
	     int count=0;
	     for(CoreLabel coreLabel : coreLabelList) {
	    	String pos= coreLabel.get(CoreAnnotations.PartOfSpeechAnnotation.class);
	    	
	    	if(pos.startsWith("NN")) {
	    		count++;
	    	}
	     }
	     
	 return count;
	}
		
	
}
