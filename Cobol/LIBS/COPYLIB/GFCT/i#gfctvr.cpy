      *****************************************************************
      * NOME DA INC - I#GFCTVR                                        *
      * DESCRICAO   - MOVIMENTO ONLINE - TARIFAS                      *
      *               COMBO                                           *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                             *
      * DATA        - 17.11.2006                                      *
      * RESPONSAVEL - FRANCISCO LOURENCO - PROCWORK - TEL 4152-4352   *
      *****************************************************************
      *
       01  GFCTVR-ENTRADA.
           03  GFCTVR-LL                            PIC S9(004) COMP.
           03  GFCTVR-ZZ                            PIC S9(004) COMP.
           03  GFCTVR-TRANSACAO                     PIC  X(009).
           03  GFCTVR-FUNCAO                        PIC  X(008).
           03  GFCTVR-DADOS-RST.
               05  GFCTVR-FILTRO.
                   07 GFCTVR-FT-CCJTO-SERVC         PIC  9(003).
                   07 GFCTVR-FT-CSERVC-TARIF        PIC  9(005).
                   07 GFCTVR-FT-DATA-REFER          PIC  X(010).
               05  GFCTVR-QTDE-OCOR                 PIC  9(003).
               05  GFCTVR-QTDE-TOT-REG              PIC  9(006).
               05  GFCTVR-FUNC-BDSCO                PIC  X(007).
               05  GFCTVR-PONTEIRO.
                   07 GFCTVR-PT-CCJTO-SERVC         PIC  9(003).
                   07 GFCTVR-PT-CSERVC-TARIF        PIC  9(005).
               05  GFCTVR-FIM                       PIC  X(001).
               05  GFCTVR-TIPO-VIGENCIA             PIC  X(001).
      *            'T' - TODOS (VIGENTES E NAO VIGENTES)
      *            'V' - VIGENTES
      *            'N' - NAO VIGENTES
      *            'P' - VIGENTES OU FUTURAS
      *            'H' - HISTORICO
      *            'F' - FUTURAS
      *            'X' - VIGENTES E PASSADAS
               05  GFCTVR-TIPO-DATA                 PIC  9(001).
      *            0 - DATA FORNECIDA PELA INC (GFCTVR-FT-DATA-REFER)
      *            1 - DATA ANTERIOR PARM_DATA_PROCM
      *            2 - DATA ATUAL    PARM_DATA_PROCM
      *            3 - DATA PROXIMA  PARM_DATA_PROCM
               05  FILLER                           PIC  X(034).
