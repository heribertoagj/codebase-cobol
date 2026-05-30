      *****************************************************************
      * NOME DA INC - GFCTWAM0                                        *
      * DESCRICAO   - COMBO DE CONJUNTOS DE SERVICOS                  *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                   GFCT5294  *
      * DATA        - 19.03.2010                                      *
      * RESPONSAVEL - WELLEN FRANCA - SONDAPROCWORK                   *
      *****************************************************************
      *
       01  WAM0-ENTRADA.
           03  WAM0-LL                              PIC S9(004) COMP.
           03  WAM0-ZZ                              PIC S9(004) COMP.
           03  WAM0-TRANSACAO                       PIC  X(009).
           03  WAM0-FUNCAO                          PIC  X(008).
           03  WAM0-DADOS-RST.
               05  WAM0-FILTRO.
                   07 WAM0-FT-CJTO-SERVC            PIC  9(003).
                   07 WAM0-FT-CAGPTO                PIC  9(003).
               05  WAM0-QTDE-OCOR                   PIC  9(003).
               05  WAM0-QTDE-TOT-REG                PIC  9(006).
               05  WAM0-FUNC-BDSCO                  PIC  X(007).
               05  WAM0-PONTEIRO.
                   07 WAM0-PT-CJTO-SERVC            PIC  9(003).
               05  WAM0-FIM                         PIC  X(001).
               05  WAM0-TIPO-VIGENCIA               PIC  X(001).
      *            'T' - TODOS (VIGENTES E NAO VIGENTES)
      *            'V' - VIGENTES
      *            'N' - NAO VIGENTES
      *            'P' - VIGENTES OU FUTURAS
      *            'H' - HISTORICO
      *            'F' - FUTURAS
      *            'X' - VIGENTES E HISTORICO
               05  WAM0-TIPO-DATA                   PIC  9(001).
      *            '0' - DATA FORNECIDA PELA INC (WAM0-DATA-REFER)
      *            '1' - DATA ANTERIOR PARM_DATA_PROCM
      *            '2' - DATA ATUAL    PARM_DATA_PROCM
      *            '3' - DATA PROXIMA  PARM_DATA_PROCM
               05  WAM0-DATA-REFER                  PIC  X(010).
               05  FILLER                           PIC  X(041).
