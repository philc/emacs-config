;; The format of this file is document hered:
;; https://github.com/minad/tempel/

fundamental-mode

(todo "TODO(philc): ")
(note "NOTE(philc): ")

js-mode

(deb "console.log(\">>> " (s text) "\", " text ");" q)
(fn "(" p ") => {" n> q n "};")
(function "function " p "(" p ") {" n> q n "};")
(log "console.log(" q ");")
(qs "document.querySelector(\"" q "\");")
(context "context(\"" p "\", () => {" n> q n "});")
(should "should(\"" p "\", () => {" n> q n "});")

emacs-lisp-mode

(deb "(progn (print \">>> " (s text) "\") (prin1 " text " t))" q)

css-mode

(out "outline: 1px solid blue;")
(bb "border: 1px solid blue;")
(br "border: 1px solid red;")
(bc "background-color: ")
(font "font-family: \"Helvetica Neue\", \"Lucida Grande\", Helvetica, Arial, Verdana, sans-serif;")

go-mode

(deb "fmt.Println(\">>> " (s text) "\", " text ")" q)
(log "fmt.Println(\">>> " (s text) "\", " text ")" q)
(err "if err != nil {" n> "panic(err)" q n "}" n)
(print "fmt.Println(\"" q "\")")
(for "for " (s text) " := 0; " text " < " p "; " text "++ {" n> q n> "}")

html-mode

(html "<!DOCTYPE html>" n "<html>" n n "<head>" n> "<meta charset=\"UTF-8\">" n>
      "<title>" p "</title>" n "</head>"
      n n "<body>" n> q n "</body>" n " </html>")
(body "<body>" n> q n "</body>")
(br "<br>")
(code "<code>" n> q n "</code>")
(script "<script>" n> q n "</script>")
(div "<div>" n> q n "</div>")

(form "<form>" n> q n "</div>")
(head "<head>" n> q n "</head>")
(h1 "<h1>" q "</h1>")
(h1 "<h2>" q "</h2>")
(h1 "<h3>" q "</h3>")
(h1 "<h4>" q "</h4>")
(h1 "<h5>" q "</h5>")
(h1 "<h6>" q "</h6>")
(hr "<hr>")
(a "<a href=\"" p "\">" q "</a>")
(img "<img src=\"" q "\" />")
(input "<input type=\"" p "\" name=\"" q "\" />")
(link "<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\" href=\"" q "\" />")
(ol "<ol>" n> q n "</ol>")
(ul "<ul>" n> q n "</ul>")
(h1 "<li>" q "</li>")
(lorem "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc sed convallis augue. Integer sit amet risus sit amet dui tempor finibus id et erat. Nunc varius turpis est, ut tincidunt ligula finibus vitae. Proin tincidunt felis at lectus tempus, quis feugiat metus aliquet. Mauris vel egestas odio. Donec id sem euismod, placerat eros vel, luctus est. Ut quis mi mauris. Vivamus sagittis efficitur sodales. Sed accumsan bibendum nibh.")

(p "<p>" q "</p>")
(pre "<pre>" n> q n "</pre>")
(quote "<blockquote>" n> q n "</blockquote>")
(span "<span>" q "</span>")

(style "<style type=\"text/css\" media=\"screen\">" n> q n "</style>")

(table "<table>" n> "<tr>" n> "<td>" q n "</td>" n "</tr>" n "</table>")
(tbody "<tbody>" n> "<tr>" n> "<td>" q n "</td>" n "</tr>" n "</tbody>")
(thead "<thead>" n> "<tr>" n> "<th>" q n "</th>" n "</tr>" n "</thead>")
(td "<td>" q "</td>")
(th "<th>" q "</th>")
(tr "<tr>" n> "<td>" n> q n "</td>" n "</tr>")

(title "<title>" q "</title>")

ruby-mode

(deb "puts \">>> " (s text) " #{ " text ".inspect}\"" q)

clojure-mode

(deb "(println \">>> " (s text) "\" " text ")" q)
(ns "(ns " q n> "(:require [chesire.core :as json])")

rust-mode

(deb "println!(\">>> " (s text) " {:?}\", " text ");" q)
