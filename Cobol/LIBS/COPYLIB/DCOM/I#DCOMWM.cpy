
       01  CBCOEB-DADOS-IO.
           03  CBCOEB-AREA-ENTRADA.
               05  CBCOEB-TPO-RETOR       PIC  X(001).
               05  CBCOEB-RESTART         PIC  X(001).
               05  CBCOEB-CBCO            PIC S9(003) COMP-3.
               05  CBCOEB-CAG-BCRIA       PIC S9(005) COMP-3.
           03  CBCOEB-AREA-SAIDA.
               05  CBCOEB-COD-RETOR       PIC  9(002).
               05  CBCOEB-COD-DB2         PIC S9(004).
               05  CBCOEB-IFNCAO          PIC  X(010).
               05  CBCOEB-NM-TAB          PIC  X(018).
               05  CBCOEB-LOCAL-ERR       PIC  X(004).
               05  CBCOEB-QTD-OCORR       PIC  9(002).
               05  CBCOEB-OCORRENCIAS     OCCURS 50 TIMES DEPENDING
                                          CBCOEB-QTD-OCORR.
                   07  CBCOEB-CBCO-CAG    PIC  9(005).
                   07  CBCOEB-CDIG-AG     PIC  X(001).
                   07  CBCOEB-IBCO-IAG    PIC  X(040).
                   07  CBCOEB-CMUN-IBGE   PIC  9(007).

