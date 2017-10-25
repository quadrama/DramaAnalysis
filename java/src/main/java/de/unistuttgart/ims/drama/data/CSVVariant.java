package de.unistuttgart.ims.drama.data;

import java.io.IOException;

import org.apache.commons.csv.CSVPrinter;
import org.apache.uima.fit.util.JCasUtil;
import org.apache.uima.jcas.JCas;

import de.tudarmstadt.ukp.dkpro.core.api.segmentation.type.Token;
import de.unistuttgart.ims.drama.api.Drama;
import de.unistuttgart.ims.drama.api.Speaker;
import de.unistuttgart.ims.drama.api.Utterance;
import de.unistuttgart.ims.drama.util.DramaUtil;

public enum CSVVariant {
	UtterancesWithTokens;

	public void header(CSVPrinter p) throws IOException {
		switch (this) {
		default:
			p.printRecord("corpus", "drama", "begin", "end", "Speaker.figure_surface", "Speaker.figure_id",
					"Token.surface", "Token.pos", "Token.lemma", "length");
		}
	}

	public void convert(JCas jcas, CSVPrinter p) throws IOException {
		switch (this) {
		default:
			this.convertUtterancesWithTokens(jcas, p);
		}

	}

	private void convertUtterancesWithTokens(JCas jcas, CSVPrinter p) throws IOException {
		Drama drama = JCasUtil.selectSingle(jcas, Drama.class);
		int length = JCasUtil.select(jcas, Token.class).size();
		for (Utterance utterance : JCasUtil.select(jcas, Utterance.class)) {
			for (Speaker speaker : DramaUtil.getSpeakers(utterance)) {
				for (int i = 0; i < speaker.getCastFigure().size(); i++) {
					for (Token token : JCasUtil.selectCovered(Token.class, utterance)) {
						p.print(drama.getCollectionId());
						p.print(drama.getDocumentId());
						p.print(speaker.getCastFigure(i).getNames(0));
						p.print(speaker.getCastFigure(i).getXmlId(0));
						p.print(token.getCoveredText());
						p.print(token.getPos().getPosValue());
						p.print(token.getLemma().getValue());
						p.print(length);
						p.println();
					}
				}
			}
		}
	}

}
