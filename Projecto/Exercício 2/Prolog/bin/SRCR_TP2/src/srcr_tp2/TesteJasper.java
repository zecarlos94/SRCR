/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package srcr_tp2;
import se.sics.jasper.*;
/**
 *
 * @author zecarlos
 */
public class TesteJasper {
     public void queryProlog() {
  String argv[]=null;
  SICStus sp;
  SPPredicate pred;
  SPTerm from, to, way;
  SPQuery query;
  int i;
    
  try 
    {
      sp = new SICStus(argv,null);

      sp.load("exercicio2.pl");
    
      pred = new SPPredicate(sp, "voa", 1, "");
      way = new SPTerm(sp).putVariable();

      query = sp.openQuery(pred, new SPTerm[] { way });
    
      while (query.nextSolution())
        {
          System.out.println(way.toString());
        }
      System.out.println("Fim da Query pretendida");
    }
  catch ( Exception e )
    {
      e.printStackTrace();
    }
  }
}
