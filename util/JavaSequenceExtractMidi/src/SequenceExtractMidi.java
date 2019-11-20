
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.sound.midi.MetaEventListener;
import javax.sound.midi.MetaMessage;
import javax.sound.midi.MidiChannel;
import javax.sound.midi.MidiDevice;
import javax.sound.midi.MidiDevice.Info;
import javax.sound.midi.MidiEvent;
import javax.sound.midi.MidiMessage;
import javax.sound.midi.MidiSystem;
import javax.sound.midi.Sequence;
import javax.sound.midi.Sequencer;
import javax.sound.midi.ShortMessage;
import javax.sound.midi.Synthesizer;
import javax.sound.midi.Track;


/**
 *
 * @author leo
 */
public class SequenceExtractMidi {

    public static void main(String[] args) throws Exception {
        for (Info info : MidiSystem.getMidiDeviceInfo()) {
            System.out.println(info);
        }
        System.out.println("------------------");
        
        MidiDevice midiDevice = MidiSystem.getMidiDevice(MidiSystem.getMidiDeviceInfo()[2]);
        
        List<Integer> notes = new ArrayList<>();
        
        Synthesizer synthesizer = MidiSystem.getSynthesizer();
        
        synthesizer.open();
        
        // se for percussao, pegar o canal 9
        MidiChannel midiChannel = synthesizer.getChannels()[0];
        
        midiChannel.programChange(1);
        // Sequence sequence = MidiSystem.getSequence(SequenceTest.class.getResourceAsStream("moonlight_sonata.mid"));
        Sequence sequence = MidiSystem.getSequence(SequenceExtractMidi.class.getResourceAsStream("vk.mid"));
        //Sequence sequence = MidiSystem.getSequence(SequenceTest.class.getResourceAsStream("kingsv.mid"));
        
        Sequencer sequencer = MidiSystem.getSequencer();
        sequencer.setSequence(sequence);
        //sequencer.open();
        //sequencer.start();
        
        //sequencer.addMetaEventListener(new MetaEventListener() {
        //    @Override
        //    public void meta(MetaMessage meta) {
        //        System.out.println(new String(meta.getMessage()));
        //    }
        //});
//        
//        int midiindex = 0;
//        while (sequencer.isRunning()) {
//            
//            MidiEvent event = sequencer.getSequence().getTracks()[6].get(midiindex);
//            if (event.getTick() > sequencer.getTickPosition()) {
//                continue;
//            }
//            midiindex++;
//            
//        MidiMessage midiMessage = event.getMessage();
//        switch (midiMessage.getStatus() & ShortMessage.NOTE_ON) {
//            case ShortMessage.NOTE_ON:
//                int note = (int) (midiMessage.getMessage()[1] & 0xff);
//                System.out.println("tick: "+ sequencer.getTickPosition() + " note_on: " + note);
//                break;
//            case ShortMessage.NOTE_OFF:
//                //int note2 = (int) (midiMessage.getMessage()[1] & 0xff);
//                //System.out.println("tick: "+ sequencer.getTickPosition() + " note_off: " + note2);
//                //break;
//        }
//
//            // System.out.println("tick position: " + sequencer.getTickPosition() + " event: " + event.getMessage().getStatus());
//            Thread.sleep(1);
//        }
//        

        
        
        System.out.println("resolution: " + sequence.getResolution()); // como obter o tick / segundo ?
        
        Map<Long, List<MidiEvent>> events = new HashMap<Long, List<MidiEvent>>();
        
        int maxTracks = sequence.getTracks().length;
        //maxTracks = 4;
        //int tracks[] = { 2, 3, 6 };
        //for (int ti = 0; ti<3; ti++) {
            int t= 6; // 6 = drums
            Track track = sequence.getTracks()[t];
            
            for (int i = 0; i < track.size(); i++) {
                MidiEvent me = track.get(i);
                MidiMessage midiMessage = me.getMessage();
                
                Long tick = me.getTick();
                List<MidiEvent> list = events.get(tick);
                if (list == null && ((midiMessage.getStatus() & ShortMessage.NOTE_ON) == ShortMessage.NOTE_ON)) {
                    list = new ArrayList<MidiEvent>();
                    events.put(tick, list);
                    list.add(me);
                }
                
                //MidiMessage message = new ShortMessage(ShortMessage.NOTE_OFF, 0, 0);
                //MidiEvent noff = new MidiEvent(message, tick + 2);
                //list.add(noff);
            }
        //}
        
        Long tick = 0l;
        while (tick  <= sequence.getTickLength()) {
            List<MidiEvent> list = events.get(tick);
            if (list != null) {
                for (MidiEvent me : list) {
                    MidiMessage midiMessage = me.getMessage();
                    
                    //System.out.print("midi event: status: " + midiMessage.getStatus() + " length: " + midiMessage.getLength() + " tick: "+ me.getTick() + " bytes: ");
                    //for (byte b : midiMessage.getMessage()) {
                    //    System.out.print((int) (b & 0xff) + " ");
                    //}
                    
                    switch (midiMessage.getStatus() & ShortMessage.NOTE_ON) {
                        case ShortMessage.NOTE_ON:
                            int note = (int) (midiMessage.getMessage()[1] & 0xff);
                            int velocity = (int) (midiMessage.getMessage()[2] & 0xff);
                            midiChannel.noteOn(note, velocity);
                            System.out.println("tick: "+ tick + " note_on: " + note);
                            notes.add(note);
                            break;
                        case ShortMessage.NOTE_OFF:
                            int note2 = (int) (midiMessage.getMessage()[1] & 0xff);
                            //int velocity2 = (int) (midiMessage.getMessage()[2] & d0xff);
                            midiChannel.noteOff(note2);
                            System.out.println("tick: "+ tick + " note_off: " + note2);
                            notes.add(254);
                            break;
                    }
                    System.out.println();
                }
            }
            else {
                // ignore
                notes.add(255);
            }
            //Thread.sleep(20);
            //System.out.println("----------------------");
            tick += 4;
        }
        
        
        System.out.println("music size: " + notes.size());
        for (int i = 0; i < notes.size(); i++) {
            int noteInt = notes.get(i);
            String noteStr = "00" + Integer.toHexString(noteInt);
            noteStr = noteStr.substring(noteStr.length() - 2, noteStr.length());
            noteStr = "0" + noteStr + "h";
            
            if (i % 16 == 0) {
                System.out.print("\r\n\t\t\tdb ");
            }

            System.out.print(noteStr + (i % 16 == 15 ? "" : ", "));
        }
        
        System.out.print("");
    }
    
    
}
