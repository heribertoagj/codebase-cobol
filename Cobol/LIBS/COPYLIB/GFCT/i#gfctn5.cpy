      ******************************************************************
      * NOME DA INC - I#GFCTN5                                         *
      * DESCRICAO   - COMBO DE AGRUPAMENTO - TABELA GFCTB001           *
      *               COMUNICACAO WEB X IMS - ENTRADA                  *
      * TAMANHO     - 100                                              *
      * DATA        - 10/03/2006                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352        *
      ******************************************************************
      *
       01  GFCTN5-ENTRADA.
           03  GFCTN5-LL                            PIC S9(004) COMP.
           03  GFCTN5-ZZ                            PIC S9(004) COMP.
           03  GFCTN5-TRANSACAO                     PIC  X(009).
           03  GFCTN5-FUNCAO                        PIC  X(008).
           03  GFCTN5-DADOS-RST.
               05  GFCTN5-FILTRO.
                   07 GFCTN5-DATA-REFER             PIC  X(010).
               05  GFCTN5-QTDE-OCOR                 PIC  9(003).
               05  GFCTN5-QTDE-TOT-REG              PIC  9(006).
               05  GFCTN5-FUNC-BDSCO                PIC  X(007).
               05  GFCTN5-PONTEIRO.
                   07 GFCTN5-PONTEIRO-AGRUP         PIC  9(003).
                   07 GFCTN5-PONTEIRO-DATA          PIC  X(010).
               05  GFCTN5-FIM                       PIC  X(001).
               05  GFCTN5-TIPO-VIGENCIA             PIC  X(001).
      *            'T' - TODOS
      *            'P' - VIGENTES OU FUTURAS
               05  GFCTN5-TIPO-DATA                 PIC  9(001).
      *            '0' - DATA FORNECIDA PELA INC (GFCTN5-DATA-REFER)
      *            '1' - DATA ANTERIOR PARM_DATA_PROCM
      *            '2' - DATA ATUAL    PARM_DATA_PROCM
      *            '3' - DATA PROXIMA  PARM_DATA_PROCM
               05  FILLER                           PIC  X(037).
