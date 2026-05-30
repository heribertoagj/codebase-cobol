      *****************************************************************
      * NOME DA INC - I#GFCTVS                                        *
      * DESCRICAO   - MOVIMENTO ONLINE - TARIFAS                      *
      *               COMBO                                           *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * DATA        - 17.11.2006                                      *
      * RESPONSAVEL - FRANCISCO LOURENCO - PROCWORK - TEL 4152-4352   *
      *****************************************************************
      *
       01  GFCTVS-SAIDA.
           03  GFCTVS-LL                            PIC S9(004) COMP.
           03  GFCTVS-ZZ                            PIC S9(004) COMP.
           03  GFCTVS-TRANSACAO                     PIC  X(009).
           03  GFCTVS-FUNCAO                        PIC  X(008).
           03  GFCTVS-FILTRO.
               05  GFCTVS-FT-CCJTO-SERVC            PIC  9(003).
               05  GFCTVS-FT-CSERVC-TARIF           PIC  9(005).
               05  GFCTVS-FT-DATA-REFER             PIC  X(010).
           03  GFCTVS-QTDE-OCOR                     PIC  9(003).
           03  GFCTVS-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTVS-FUNC-BDSCO                    PIC  X(007).
           03  GFCTVS-PONTEIRO.
               05  GFCTVS-PT-CCJTO-SERVC            PIC  9(003).
               05  GFCTVS-PT-CSERVC-TARIF           PIC  9(005).
           03  GFCTVS-ERRO                          PIC  9(001).
           03  GFCTVS-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTVS-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTVS-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTVS-FIM                           PIC  X(001).
           03  GFCTVS-TIPO-VIGENCIA                 PIC  X(001).
      *        'T' - TODOS (VIGENTES E NAO VIGENTES)
      *        'V' - VIGENTES
      *        'N' - NAO VIGENTES
      *        'P' - VIGENTES OU FUTURAS
      *        'H' - HISTORICO
      *        'F' - FUTURAS
      *        'X' - VIGENTES E PASSADAS
           03  GFCTVS-TIPO-DATA                     PIC  9(001).
      *        0 - DATA FORNECIDA PELA INC (GFCTVR-DATA-REFER)
      *        1 - DATA ANTERIOR PARM_DATA_PROCM
      *        2 - DATA ATUAL    PARM_DATA_PROCM
      *        3 - DATA PROXIMA  PARM_DATA_PROCM
           03  FILLER                               PIC  X(008).
           03  GFCTVS-DADOS.
               05  FILLER                           OCCURS 18.
                   07  GFCTVS-COD-TARIFA            PIC  9(005).
                   07  GFCTVS-DESC-TARIFA           PIC  X(040).
                   07  GFCTVS-TIPO-VGCIA-TARIFA     PIC  X(001).
      *                'T' - TODOS (VIGENTES E NAO VIGENTES)
      *                'V' - VIGENTES
      *                'N' - NAO VIGENTES
      *                'P' - VIGENTES OU POSTERIORES
      *                'H' - HISTORICO
      *                'F' - FUTURAS
      *                'X' - VIGENTES E PASSADAS
                   07  GFCTVS-TIPO-TARIFA           PIC  9(001).
      *                1 - INDIVIDUAL
      *                2 - PACOTE
      *                3 - COMBINADA
