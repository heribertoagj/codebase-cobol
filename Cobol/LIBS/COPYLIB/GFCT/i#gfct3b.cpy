      *****************************************************************
      * NOME DA INC - I#GFCT3B                                        *
      * DESCRICAO   - MOVIMENTO ONLINE - TARIFAS                      *
      *               COMBO                                           *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * DATA        - 20.12.2004                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41956693       *
      *****************************************************************
      *
       01  GFCT3B-SAIDA.
           03  GFCT3B-LL                            PIC S9(004) COMP.
           03  GFCT3B-ZZ                            PIC S9(004) COMP.
           03  GFCT3B-TRANSACAO                     PIC  X(009).
           03  GFCT3B-FUNCAO                        PIC  X(008).
           03  GFCT3B-FILTRO.
               05  GFCT3B-CSERVC-TARIF              PIC  9(005).
               05  GFCT3B-DINIC-VGCIA               PIC  X(010).
           03  GFCT3B-QTDE-OCOR                     PIC  9(003).
           03  GFCT3B-QTDE-TOT-REG                  PIC  9(006).
           03  GFCT3B-FUNC-BDSCO                    PIC  X(007).
           03  GFCT3B-TERMINAL                      PIC  X(008).
           03  GFCT3B-PONTEIRO                      PIC  9(005).
           03  GFCT3B-ERRO                          PIC  9(001).
           03  GFCT3B-COD-MSG-ERRO                  PIC  9(007).
           03  GFCT3B-COD-SQL-ERRO                  PIC  9(003).
           03  GFCT3B-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCT3B-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(026).
           03  GFCT3B-DADOS.
               05  FILLER                           OCCURS 18.
                   07  GFCT3B-COD-TARIFA            PIC  9(005).
                   07  GFCT3B-DESC-TARIFA           PIC  X(040).
                   07  GFCT3B-TIPO-VIGENCIA         PIC  X(001).
      *                'T' - TODOS (VIGENTES E NAO VIGENTES)
      *                'V' - VIGENTES
      *                'N' - NAO VIGENTES
      *                'P' - VIGENTES OU POSTERIORES
      *                'H' - HISTORICO
      *                'F' - FUTURAS
      *                'X' - VIGENTES E PASSADAS
      *---------------------------------------------------------
