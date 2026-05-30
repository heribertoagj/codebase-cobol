      *****************************************************************
      * NOME DA INC - GFCTWAAN                                        *
      * DESCRICAO   - MOVIMENTO ONLINE - TARIFAS                      *
      *               COMBO  -  GFCT5588                              *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * DATA        - 07.07.2008                                      *
      * RESPONSAVEL - VALERIA  - PROCWORK                             *
      *****************************************************************
       01  WAAN-SAIDA.
           03  WAAN-LL                              PIC S9(004) COMP.
           03  WAAN-ZZ                              PIC S9(004) COMP.
           03  WAAN-TRANSACAO                       PIC  X(009).
           03  WAAN-FUNCAO                          PIC  X(008).
           03  WAAN-FILTRO.
               05  WAAN-FT-CCJTO-SERVC              PIC  9(003).
               05  WAAN-FT-AGRUPAMENTO              PIC  9(003).
               05  WAAN-FT-CSERVC-TARIF             PIC  9(005).
               05  WAAN-FT-DATA-REFER               PIC  X(010).
           03  WAAN-QTDE-OCOR                       PIC  9(007).
           03  WAAN-QTDE-TOT-REG                    PIC  9(009).
           03  WAAN-FUNC-BDSCO                      PIC  X(007).
           03  WAAN-PONTEIRO.
               05  WAAN-PT-CCJTO-SERVC              PIC  9(003).
               05  WAAN-PT-CSERVC-TARIF             PIC  9(005).
           03  WAAN-ERRO                            PIC  9(001).
           03  WAAN-COD-MSG-ERRO                    PIC  9(007).
           03  WAAN-COD-SQL-ERRO                    PIC S9(003).
           03  WAAN-DESC-MSG-ERRO                   PIC  X(079).
           03  WAAN-FIM                             PIC  X(001).
           03  WAAN-TIPO-VIGENCIA                   PIC  X(001).
      *        'T' - TODOS (VIGENTES E NAO VIGENTES)
      *        'V' - VIGENTES
      *        'N' - NAO VIGENTES
      *        'P' - VIGENTES OU FUTURAS
      *        'H' - HISTORICO
      *        'F' - FUTURAS
      *        'X' - VIGENTES E PASSADAS
           03  WAAN-TIPO-DATA                       PIC  9(001).
      *        0 - DATA FORNECIDA PELA INC (GFCTVR-DATA-REFER)
      *        1 - DATA ANTERIOR PARM_DATA_PROCM
      *        2 - DATA ATUAL    PARM_DATA_PROCM
      *        3 - DATA PROXIMA  PARM_DATA_PROCM
           03  FILLER                                 PIC  X(012).
           03  WAAN-DADOS.
               05  FILLER                           OCCURS 16.
                   07  WAAN-COD-TARIFA              PIC  9(005).
                   07  WAAN-DESC-TARIFA             PIC  X(040).
                   07  WAAN-TIPO-VGCIA-TARIFA       PIC  X(001).
      *                'T' - TODOS (VIGENTES E NAO VIGENTES)
      *                'V' - VIGENTES
      *                'N' - NAO VIGENTES
      *                'P' - VIGENTES OU POSTERIORES
      *                'H' - HISTORICO
      *                'F' - FUTURAS
      *                'X' - VIGENTES E PASSADAS
                   07  WAAN-TIPO-TARIFA             PIC  9(001).
      *                1 - INDIVIDUAL
      *                2 - PACOTE
      *                3 - COMBINADA
                   07  WAAN-PRMSS-AGNDA             PIC  9(001).
                   07  WAAN-PRMSS-VALOR             PIC  9(001).
                   07  WAAN-PRMSS-PERCENT           PIC  9(001).
                   07  WAAN-PRMSS-FRANQ             PIC  9(001).
                   07  WAAN-PRMSS-PERDC             PIC  9(001).
