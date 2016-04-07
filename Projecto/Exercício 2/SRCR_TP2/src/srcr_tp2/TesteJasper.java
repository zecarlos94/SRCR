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
     public static void main(String argv[]) {

  SICStus sp;
  SPPredicate pred;
  SPTerm from, to, way;
  SPQuery query;
  int i;
    
  try 
    {
      sp = new SICStus(argv,null);

      sp.load("train.ql");
    
      pred = new SPPredicate(sp, "connected", 4, "");
      to = new SPTerm(sp, "Orebro");
      from = new SPTerm(sp, "Stockholm");
      way = new SPTerm(sp).putVariable();

      query = sp.openQuery(pred, new SPTerm[] { from, to, way, way });
    
      while (query.nextSolution())
        {
          System.out.println(way.toString());
        }
    }
  catch ( Exception e )
    {
      e.printStackTrace();
    }
  }
}
