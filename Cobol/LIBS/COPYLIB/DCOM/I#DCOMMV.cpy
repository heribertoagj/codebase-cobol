      ******************************************************************
      * SISTEMA : DCOM - I#DCOMMV                                      *
      * TAMANHO : 0256 BYTES                                           *
      * ARQUIVO : TRANSMISSAO DA PRODUCAO DIARIA DESCONTO COMERCIAL    *
      ******************************************************************
       01  MV-REGISTRO.
      *    --->> CHAVE
  1 10     03  MV-DMOVTC-PROD-DIA           PIC X(10).
 11  5     03  MV-CJUNC-DEPDC               PIC 9(05).
 16  5     03  MV-CPOSTO-SERVC              PIC 9(05).
 21  2     03  MV-CTPO-PAB                  PIC X(02).
 23  3     03  MV-SEGMENTO-CLIENTE          PIC 9(03).
 26  3     03  MV-CPRODT                    PIC 9(03).
 29  3     03  MV-CSPROD                    PIC 9(03).
 32  2     03  MV-CINDCD-CONTR-CONVE        PIC X(02).
 34  9     03  MV-CCONTR-CONVE-DESC         PIC 9(09).
 43  3     03  MV-CVRSAO-CONTR-CONVE        PIC 9(03).
      *    --->> ATRIBUTOS
 46 17     03  MV-TTAL-VLR-CONTRATO         PIC 9(15)V9(02).
 63 17     03  MV-VALOR-IOF                 PIC 9(15)V9(02).
 80 17     03  MV-VALOR-TAC                 PIC 9(15)V9(02).
 97 17     03  MV-VLR-REG-TITULO            PIC 9(15)V9(02).
114  5     03  MV-PZO-MEDIO-CONTRATO        PIC 9(05).
119  5     03  MV-PZO-MEDIO-TITULOS         PIC 9(05).
124  5     03  MV-TX-MEDIA-ANUAL            PIC 9(03)V9(02).
129  5     03  MV-TX-MEDIA-MENSAL           PIC 9(03)V9(02).
134 15     03  MV-QTD-TTAL-CONTRATO         PIC 9(15).
149 15     03  MV-QTD-TTAL-TITULOS          PIC 9(15).
164 15     03  MV-QTD-TTAL-CHEQUES          PIC 9(15).
179 78     03  FILLER                       PIC X(78).
