      *****************************************************************
      * NOME DA INC - GFCTWAAM                                        *
      * DESCRICAO   - MOVIMENTO ONLINE - TARIFAS                      *
      *               COMBO  -  GFCT5588                              *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                             *
      * DATA        - 07.07.2008                                      *
      * RESPONSAVEL - VALERIA  - PROCWORK                             *
      *****************************************************************
      *
       01  WAAM-ENTRADA.
           03  WAAM-LL                              PIC S9(004) COMP.
           03  WAAM-ZZ                              PIC S9(004) COMP.
           03  WAAM-TRANSACAO                       PIC  X(009).
           03  WAAM-FUNCAO                          PIC  X(008).
           03  WAAM-DADOS-RST.
               05  WAAM-FILTRO.
                   07 WAAM-FT-CCJTO-SERVC           PIC  9(003).
                   07 WAAM-FT-AGRUPAMENTO           PIC  9(003).
                   07 WAAM-FT-CSERVC-TARIF          PIC  9(005).
                   07 WAAM-FT-DATA-REFER            PIC  X(010).
               05  WAAM-QTDE-OCOR                   PIC  9(007).
               05  WAAM-QTDE-TOT-REG                PIC  9(009).
               05  WAAM-FUNC-BDSCO                  PIC  X(007).
               05  WAAM-PONTEIRO.
                   07 WAAM-PT-CCJTO-SERVC           PIC  9(003).
                   07 WAAM-PT-CSERVC-TARIF          PIC  9(005).
               05  WAAM-FIM                         PIC  X(001).
               05  WAAM-TIPO-VIGENCIA               PIC  X(001).
      *            'T' - TODOS (VIGENTES E NAO VIGENTES)
      *            'V' - VIGENTES
      *            'N' - NAO VIGENTES
      *            'P' - VIGENTES OU FUTURAS
      *            'H' - HISTORICO
      *            'F' - FUTURAS
      *            'X' - VIGENTES E PASSADAS
               05  WAAM-TIPO-DATA                   PIC  9(001).
      *            0 - DATA FORNECIDA PELA INC (WAAM-FT-DATA-REFER)
      *            1 - DATA ANTERIOR PARM_DATA_PROCM
      *            2 - DATA ATUAL    PARM_DATA_PROCM
      *            3 - DATA PROXIMA  PARM_DATA_PROCM
               05  FILLER                             PIC  X(024).
