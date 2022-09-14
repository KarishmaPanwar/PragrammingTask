package nlp;

import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Servlet implementation class NLPServ
 */
@WebServlet("/NLPServ")
public class NLPServ extends HttpServlet {
	private static final long serialVersionUID = 1L;
  
    public NLPServ() {
        super();
       
    }

	
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		response.getWriter().append("Served at: ").append(request.getContextPath());
	}

	
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		String data= request.getParameter("userInput");
		TextProcessing txtpro = new TextProcessing();
		int numOfWords= txtpro.getCount(data);
		int numOfSentences= txtpro.getCountSentence(data);
		int numOfPoS= txtpro.getCountPoS(data);
		System.out.println("NUMBER OF WORDS"+numOfWords);
		//response.sendRedirect("/index.html");
		PrintWriter writer = response.getWriter();
        
        // build HTML code
        String htmlRespone = "<html>";
        htmlRespone += "<h3>";
        htmlRespone += "Number Of Words: " + numOfWords + "<br/>";      
        htmlRespone += "Number Of Sentences: " + numOfSentences + "<br/>";
        htmlRespone += "Number of Part Of Speech: " + numOfPoS + "<br/>";
        htmlRespone += "</h3>";
       
        htmlRespone += "</html>";
         
        // return response
        writer.println(htmlRespone);
        writer.close();
        //request.getRequestDispatcher("result.html").forward(request, response);
	}

}
