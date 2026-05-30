      *************************************************************
      * SISTEMA : DESCONTO COMERCIAL                              *
      * ARRQUIVO: ARQUIVO DE TRANSMISSAO DO POLO VIA COBRANCA     *
      * INC     : I#DCOMRU -  LRECL = 400 BYTES                   *
      *************************************************************

       01  RU-REGISTRO.
  1  1     05  RU-TP-REG                 PIC  9(001).
  2  5     05  RU-SUBCENTRO              PIC  9(005).
  7  5     05  RU-ORIGEM-DIGITACAO       PIC  9(005).
 12  5     05  RU-CAG-BCRIA-CEDENTE      PIC  9(005).
 17  7     05  RU-CCTA-BCRIA-CEDENTE     PIC  9(007).
 24  1     05  RU-DIG-CCTA-CEDENTE       PIC  X(001).
 25  3     05  RU-CARTEIRA               PIC  9(003).
 28  4     05  RU-CAG-BCRIA-OPER         PIC  9(004).
 32  1     05  RU-DIGITO                 PIC  X(001).
 33  7     05  RU-RESERVA                PIC  X(007).
 40 10     05  RU-DT-EMISSAO             PIC  X(010).
 50  3     05  RU-CBCO-DEPOS             PIC  9(003).
 53  4     05  RU-CAG-BCRIA-DEPOS        PIC  9(004).
 57 40     05  RU-NOME-SACADO            PIC  X(040).
 97 40     05  RU-ENDERECO               PIC  X(040).
137  5     05  RU-CCEP                   PIC  9(005).
142  3     05  RU-CCEP-COMPL             PIC  9(003).
           05  RU-CGC-CPF-SACADO.
145  9         10  RU-CGC-SACDO          PIC  9(009).
154  4         10  RU-CFLIAL-SACDO       PIC  9(004).
158  2         10  RU-CCTRL-SACDO        PIC  9(002).
160 10     05  RU-SEU-NUMERO             PIC  X(010).
170 10     05  RU-DT-VENCTO              PIC  X(010).
180 15     05  RU-VLR-TITULO             PIC  9(015).
195 10     05  RU-DT-DESCONTO            PIC  X(010).
205 13     05  RU-VLR-DESCONTO           PIC  9(013).
218 13     05  RU-VLR-IOF                PIC  9(013).
231 13     05  RU-VLR-MORA-DIA           PIC  9(013).
244 13     05  RU-VLR-ABATIMENTO         PIC  9(013).
257  2     05  RU-ESPECIE                PIC  9(002).
259  1     05  RU-ACEITE                 PIC  9(001).
260  3     05  RU-COD-MOEDA              PIC  9(003).
263  2     05  FILLER                    PIC  X(002).
265 40     05  RU-NOME-SACADOR           PIC  X(040).
           05  RU-CGC-CPF-SACADOR.
305  9         10  RU-CGC-SACDOR         PIC  9(009).
314  4         10  RU-CFLIAL-SACDOR      PIC  9(004).
318  2         10  RU-CTRL-SACDOR        PIC  9(002).
320  1     05  RU-ORIGEM                 PIC  X(001).
321  4     05  RU-DANO-OPER-DESC         PIC  9(004).
325  9     05  RU-NSEQ-OPER-DESC         PIC  9(009).

      *--->  CAMPOS ADICIONAIS DO DCOM

334  4     05  RU-NUM-AUT                PIC  9(004).
338  3     05  RU-CPRODT                 PIC  9(003).
341  3     05  RU-CSPROD-DESC-COML       PIC  9(003).
344  3     05  RU-CMEIO-ENTRD-DESC       PIC  9(003).
347  1     05  RU-CINDCD-PROTE-TITLO     PIC  X(001).
348  3     05  RU-QDIA-PROTE-TITLO       PIC  9(003).
351  2     05  FILLER                    PIC  X(002).
353 13     05  RU-CEMPR-TROCAF           PIC  9(013).
366 11     05  RU-NOSSO-NUMERO           PIC  9(011).
377  3     05  RU-COCOR-REMSS            PIC  9(003).
380  3     05  RU-CMOTVO-REMSS           PIC  9(003).
383 18     05  RU-FILLER                 PIC  X(018).
