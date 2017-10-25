package de.unistuttgart.ims.drama.data;

import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.commons.csv.CSVPrinter;
import org.apache.uima.fit.util.JCasUtil;
import org.apache.uima.jcas.JCas;

import de.tudarmstadt.ukp.dkpro.core.api.segmentation.type.Token;
import de.unistuttgart.ims.drama.api.Act;
import de.unistuttgart.ims.drama.api.Drama;
import de.unistuttgart.ims.drama.api.Scene;
import de.unistuttgart.ims.drama.api.Speaker;
import de.unistuttgart.ims.drama.api.Utterance;
import de.unistuttgart.ims.drama.util.DramaUtil;

public enum CSVVariant {
	UtterancesWithTokens, Segments;

	public void header(CSVPrinter p) throws IOException {
		switch (this) {
		case Segments:
			p.printRecord("drama", "corpus", "begin.Act", "end.Act", "Number.Act", "begin.Scene", "end.Scene",
					"Number.Scene");
			break;
		default:
			p.printRecord("corpus", "drama", "begin", "end", "Speaker.figure_surface", "Speaker.figure_id",
					"Token.surface", "Token.pos", "Token.lemma", "length");
		}
	}

	public void convert(JCas jcas, CSVPrinter p) throws IOException {
		switch (this) {
		case Segments:
			this.convertSegments(jcas, p);
			break;
		default:
			this.convertUtterancesWithTokens(jcas, p);
		}

	}

	private void convertSegments(JCas jcas, CSVPrinter p) throws IOException {
		Drama drama = JCasUtil.selectSingle(jcas, Drama.class);
		if (JCasUtil.exists(jcas, Act.class)) {
			for (Act act : JCasUtil.select(jcas, Act.class)) {
				Collection<Scene> scenes = JCasUtil.selectCovered(Scene.class, act);
				if (scenes.isEmpty()) {
					p.printRecord(drama.getCollectionId(), drama.getDocumentId(), act.getBegin(), act.getEnd(),
							act.getNumber(), null, null, null);

				} else
					for (Scene scene : scenes) {
						p.printRecord(drama.getCollectionId(), drama.getDocumentId(), act.getBegin(), act.getEnd(),
								act.getNumber(), scene.getBegin(), scene.getEnd(), scene.getNumber());
					}
			}
		} else {
			Collection<Scene> scenes = JCasUtil.select(jcas, Scene.class);
			if (scenes.isEmpty()) {
				p.printRecord(drama.getCollectionId(), drama.getDocumentId(), null, null, null, null, null, null);

			} else
				for (Scene scene : scenes) {
					p.printRecord(drama.getCollectionId(), drama.getDocumentId(), null, null, null, scene.getBegin(),
							scene.getEnd(), scene.getNumber());
				}
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
