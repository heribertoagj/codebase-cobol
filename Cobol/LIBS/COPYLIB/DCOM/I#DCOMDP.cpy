      ******************************************************************
      * SISTEMA : DCOM - I#DCOMDP                                      *
      * TAMANHO : 0256 BYTES                                           *
      * ARQUIVO : PRODUCAO DIARIA (OPERACOES QUE ENTRARAM NO DIA)      *
      ******************************************************************
       01  DP-REGISTRO.
      *    --->> CHAVE
  1 10     03  DP-DMOVTC-PROD-DIA           PIC X(10).
 11  5     03  DP-CJUNC-DEPDC               PIC 9(05).
 16  5     03  DP-CPOSTO-SERVC              PIC 9(05).
 21  2     03  DP-CTPO-PAB                  PIC X(02).
 23  3     03  DP-SEGMENTO-CLIENTE          PIC 9(03).
 26  3     03  DP-CPRODT                    PIC 9(03).
 29  3     03  DP-CSPROD                    PIC 9(03).
 32  2     03  DP-CINDCD-CONTR-CONVE        PIC X(02).
 34  9     03  DP-CCONTR-CONVE-DESC         PIC 9(09).
 43  3     03  DP-CVRSAO-CONTR-CONVE        PIC 9(03).
      *    --->> ATRIBUTOS
 46 17     03  DP-VLR-TTAL-A                PIC 9(15)V9(02).
 63 17     03  DP-VLR-TTAL-B                PIC 9(15)V9(02).
 80 17     03  DP-VLR-TTAL-C                PIC 9(15)V9(02).
 97 17     03  DP-VLR-TTAL-D                PIC 9(15)V9(02).
114 17     03  DP-VLR-TTAL-E                PIC 9(15)V9(02).
131 17     03  DP-VALOR-IOF                 PIC 9(15)V9(02).
148 17     03  DP-VALOR-TAC                 PIC 9(15)V9(02).
165 17     03  DP-VLR-REG-TITULO            PIC 9(15)V9(02).
182  5     03  DP-PZO-OPER                  PIC 9(05).
187  5     03  DP-PZO-TITL                  PIC 9(05).
192 15     03  DP-QTDE-TTAL-CONTRATO        PIC 9(15).
207 15     03  DP-QTDE-TTAL-TITULOS         PIC 9(15).
222 15     03  DP-QTDE-TTAL-CHEQUES         PIC 9(15).
237  9     03  DP-TX-ANUAL-OPER             PIC 9(03)V9(06).
246  9     03  DP-TAXA-MES-OPER             PIC 9(03)V9(06).
255  2     03  FILLER                       PIC X(02).
