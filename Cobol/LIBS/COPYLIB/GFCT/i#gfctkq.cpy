      *****************************************************************
      * NOME DA INC - I#GFCTKQ                                        *
      * DESCRICAO   - COMBO DE TARIFAS GERENCIADAS                    *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * DATA        - 07.06.2005                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41956693       *
      *****************************************************************
      *
       01  GFCTKQ-SAIDA.
           03  GFCTKQ-LL                            PIC S9(004) COMP.
           03  GFCTKQ-ZZ                            PIC S9(004) COMP.
           03  GFCTKQ-TRANSACAO                     PIC  X(009).
           03  GFCTKQ-FUNCAO                        PIC  X(008).
           03  GFCTKQ-FILTRO.
               05  GFCTKQ-CSERVC-TARIF              PIC  9(005).
               05  GFCTKQ-DATA-REFER                PIC  X(010).
           03  GFCTKQ-QTDE-OCOR                     PIC  9(003).
           03  GFCTKQ-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTKQ-FUNC-BDSCO                    PIC  X(007).
           03  GFCTKQ-TERMINAL                      PIC  X(008).
           03  GFCTKQ-PONTEIRO.
               05 GFCTKQ-PONTEIRO-TARIFA            PIC  9(005).
               05 GFCTKQ-PONTEIRO-DATA              PIC  X(010).
           03  GFCTKQ-ERRO                          PIC  9(001).
           03  GFCTKQ-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTKQ-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTKQ-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTKQ-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(102).
           03  GFCTKQ-DADOS.
               05  FILLER                           OCCURS 7.
                   07  GFCTKQ-COD-TARIFA            PIC  9(005).
                   07  GFCTKQ-DESC-TARIFA           PIC  X(100).
                   07  GFCTKQ-TIPO-VIGENCIA         PIC  X(001).
      *                'T' - TODOS (VIGENTES E NAO VIGENTES)
      *                'V' - VIGENTES
      *                'N' - NAO VIGENTES
      *                'P' - VIGENTES OU POSTERIORES
      *                'H' - HISTORICO
      *                'F' - FUTURAS
