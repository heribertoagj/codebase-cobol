      ******************************************************************
      * NOME DA INC - I#GFCTL6                                         *
      * DESCRICAO   - COMBO DE AGRUPAMENTO - PERMISSAO DE ADESAO       *
      *               COMUNICACAO IMS X WEB - SAIDA                    *
      * TAMANHO     - 1010                                             *
      * DATA        - 15/02/2006                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352        *
      ******************************************************************
      *
       01  GFCTL6-SAIDA.
           03  GFCTL6-LL                            PIC S9(004) COMP.
           03  GFCTL6-ZZ                            PIC S9(004) COMP.
           03  GFCTL6-TRANSACAO                     PIC  X(009).
           03  GFCTL6-FUNCAO                        PIC  X(008).
           03  GFCTL6-FILTRO.
               05  GFCTL6-CSERVC-TARIF              PIC  9(005).
               05  GFCTL6-DATA-REFER                PIC  X(010).
           03  GFCTL6-QTDE-OCOR                     PIC  9(003).
           03  GFCTL6-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTL6-FUNC-BDSCO                    PIC  X(007).
           03  GFCTL6-PONTEIRO.
               05  GFCTL6-PONTEIRO-AGRUP            PIC  9(003).
               05  GFCTL6-PONTEIRO-DATA             PIC  X(010).
           03  GFCTL6-ERRO                          PIC  9(001).
           03  GFCTL6-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTL6-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTL6-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTL6-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(174).
           03  GFCTL6-DADOS.
               05  FILLER                           OCCURS 20.
                   07  GFCTL6-TP-AGRUP              PIC  9(003).
                   07  GFCTL6-DESC-AGRUP            PIC  X(030).
                   07  GFCTL6-TIPO-VIGENCIA         PIC  X(001).
      *                'T' - TODOS (VIGENTES E NAO VIGENTES)
      *                'V' - VIGENTES
      *                'N' - NAO VIGENTES
      *                'P' - VIGENTES OU POSTERIORES
      *                'H' - HISTORICO
      *                'F' - FUTURAS
