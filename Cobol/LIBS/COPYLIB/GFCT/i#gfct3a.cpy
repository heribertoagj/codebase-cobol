      *****************************************************************
      * NOME DA INC - I#GFCT3A                                        *
      * DESCRICAO   - MOVIMENTO ONLINE - TARIFAS                      *
      *               COMBO                                           *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                             *
      * DATA        - 20.12.2004                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41956693       *
      *****************************************************************
      *
       01  GFCT3A-ENTRADA.
           03  GFCT3A-LL                            PIC S9(004) COMP.
           03  GFCT3A-ZZ                            PIC S9(004) COMP.
           03  GFCT3A-TRANSACAO                     PIC  X(009).
           03  GFCT3A-FUNCAO                        PIC  X(008).
           03  GFCT3A-DADOS-RST.
               05  GFCT3A-FILTRO.
                   07 GFCT3A-CSERVC-TARIF           PIC  9(005).
                   07 GFCT3A-DATA-REFER             PIC  X(010).
               05  GFCT3A-QTDE-OCOR                 PIC  9(003).
               05  GFCT3A-QTDE-TOT-REG              PIC  9(006).
               05  GFCT3A-FUNC-BDSCO                PIC  X(007).
               05  GFCT3A-TERMINAL                  PIC  X(008).
               05  GFCT3A-PONTEIRO                  PIC  9(005).
               05  GFCT3A-FIM                       PIC  X(001).
               05  GFCT3A-TIPO-VIGENCIA             PIC  X(001).
      *            'T' - TODOS (VIGENTES E N O VIGENTES)
      *            'V' - VIGENTES
      *            'N' - N O VIGENTES
      *            'P' - VIGENTES OU FUTURAS
      *            'H' - HISTORICO
      *            'F' - FUTURAS
      *            'X' - VIGENTES E PASSADAS
               05  GFCT3A-TIPO-DATA                 PIC  9(001).
      *            '0' - DATA FORNECIDA PELA INC (GFCT3A-DATA-REFER)
      *            '1' - DATA ANTERIOR PARM_DATA_PROCM
      *            '2' - DATA ATUAL    PARM_DATA_PROCM
      *            '3' - DATA PROXIMA  PARM_DATA_PROCM
               05  FILLER                           PIC  X(032).
