package polyglot.a01a;

import javax.swing.*;
import java.util.*;
import java.awt.*;
import java.awt.event.ActionListener;
import polyglot.Pair;

public class GUI extends JFrame {

    private static final long serialVersionUID = -6218820567019985015L;
    private final Map<JButton,Pair<Integer,Integer>> buttons = new HashMap<>();
    private final LogicsScala logics;

    public GUI(int size, int boat) {
        this.logics = LogicsScala.apply(size,boat);
        this.setDefaultCloseOperation(EXIT_ON_CLOSE);
        this.setSize(100*size, 100*size);

        JPanel panel = new JPanel(new GridLayout(size,size));
        this.getContentPane().add(BorderLayout.CENTER,panel);

        ActionListener al = (e)->{
            final JButton bt = (JButton)e.getSource();
            final Pair<Integer,Integer> p = buttons.get(bt);
            //System.out.println("hit "+p);
            final ResultScala result = logics.hit(p.getY(), p.getX());
            final String res = ResultScalaUtils.name(result);
            if (res.equals("Won") || res.equals("Lost")){
                System.out.println(res);
                System.exit(0);
            }
            bt.setText(res.equals("Hit") ? "X" : "O");
            bt.setEnabled(false);
        };

        for (int i=0; i<size; i++){
            for (int j=0; j<size; j++){
                final JButton jb = new JButton(" ");
                jb.addActionListener(al);
                this.buttons.put(jb,new Pair<>(j,i));
                panel.add(jb);
            }
        }
        this.setVisible(true);
    }

}
