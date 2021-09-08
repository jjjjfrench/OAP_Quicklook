OAP_QUICKLOOK PDF MERGER

To save time on PDF generation, the OAP code generates PDF pages individually whenthere are more than 400 pages to generate. To combine the pages, we reccomend using Poppler. The following link demonstrates how to donwload:

https://poppler.freedesktop.org/

Navigate to the file directory in the Terminal. Use the line:

pdfunite C* outputfile.pdf

This will combine all the OAP generated files in to one PDF. An image of this code is also available in the repository.