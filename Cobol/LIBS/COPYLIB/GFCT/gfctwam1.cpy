      *****************************************************************
      * NOME DA INC - GFCTWAM1                                        *
      * DESCRICAO   - COMBO DE CONJUNTOS DE SERVICOS                  *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                 GFCT5294   *
      * DATA        - 19.03.2010                                      *
      * RESPONSAVEL - WELLEN FRANCA  - SONDAPROCWORK                  *
      *****************************************************************
      *
       01  WAM1-SAIDA.
           03  WAM1-LL                              PIC S9(004) COMP.
           03  WAM1-ZZ                              PIC S9(004) COMP.
           03  WAM1-TRANSACAO                       PIC  X(009).
           03  WAM1-FUNCAO                          PIC  X(008).
           03  WAM1-FILTRO.
               05  WAM1-FT-CJTO-SERVC               PIC  9(003).
               05  FILLER                           PIC  X(003).
           03  WAM1-QTDE-OCOR                       PIC  9(003).
           03  WAM1-QTDE-TOT-REG                    PIC  9(006).
           03  WAM1-FUNC-BDSCO                      PIC  X(007).
           03  WAM1-PONTEIRO.
               05 WAM1-PT-CJTO-SERVC                PIC  9(003).
           03  WAM1-ERRO                            PIC  9(001).
           03  WAM1-COD-MSG-ERRO                    PIC  9(007).
           03  WAM1-COD-SQL-ERRO                    PIC  9(003).
           03  WAM1-DESC-MSG-ERRO                   PIC  X(079).
           03  WAM1-FIM                             PIC  X(001).
           03  WAM1-TIPO-VIGENCIA                   PIC  X(001).
      *        'T' - TODOS (VIGENTES E NAO VIGENTES)
      *        'V' - VIGENTES
      *        'N' - NAO VIGENTES
      *        'P' - VIGENTES OU FUTURAS
      *        'H' - HISTORICO
      *        'F' - FUTURAS
      *        'X' - VIGENTES E HISTORICO
           03  WAM1-TIPO-DATA                       PIC  9(001).
      *        '0' - DATA FORNECIDA PELA INC (GFCTVP-DATA-REFER)
      *        '1' - DATA ANTERIOR PARM_DATA_PROCM
      *        '2' - DATA ATUAL    PARM_DATA_PROCM
      *        '3' - DATA PROXIMA  PARM_DATA_PROCM
           03  FILLER                               PIC  X(091).
           03  WAM1-DADOS.
               05  FILLER                           OCCURS 20.
                   07  WAM1-COD-CJTO-SERVC          PIC  9(003).
                   07  WAM1-DESC-CJTO-SERVC         PIC  X(030).
                   07  WAM1-CINDTFD-AGP             PIC  X(001).
      * S = SIM
      * N = NAO
                   07  WAM1-PRMSS-AGNDA             PIC  9(001).
                   07  WAM1-PRMSS-VALOR             PIC  9(001).
                   07  WAM1-PRMSS-PERCENT           PIC  9(001).
                   07  WAM1-PRMSS-FRANQ             PIC  9(001).
                   07  WAM1-PRMSS-PERDC             PIC  9(001).
