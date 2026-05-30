      *****************************************************************
      * NOME DA INC - I#GFCTL4                                        *
      * DESCRICAO   - COMBO DE TARIFAS - PERMISSAO DE ADESAO          *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * DATA        - 15.02.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41956693       *
      *****************************************************************
      *
       01  GFCTL4-SAIDA.
           03  GFCTL4-LL                            PIC S9(004) COMP.
           03  GFCTL4-ZZ                            PIC S9(004) COMP.
           03  GFCTL4-TRANSACAO                     PIC  X(009).
           03  GFCTL4-FUNCAO                        PIC  X(008).
           03  GFCTL4-FILTRO.
               05  GFCTL4-DATA-REFER                PIC  X(010).
           03  GFCTL4-QTDE-OCOR                     PIC  9(003).
           03  GFCTL4-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTL4-FUNC-BDSCO                    PIC  X(007).
           03  GFCTL4-PONTEIRO.
               05 GFCTL4-PONTEIRO-TARIFA            PIC  9(005).
               05 GFCTL4-PONTEIRO-DATA              PIC  X(010).
           03  GFCTL4-ERRO                          PIC  9(001).
           03  GFCTL4-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTL4-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTL4-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTL4-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(115).
           03  GFCTL4-DADOS.
               05  FILLER                           OCCURS 7.
                   07  GFCTL4-COD-TARIFA            PIC  9(005).
                   07  GFCTL4-DESC-TARIFA           PIC  X(100).
                   07  GFCTL4-TIPO-VIGENCIA         PIC  X(001).
      *                'T' - TODOS (VIGENTES E NAO VIGENTES)
      *                'V' - VIGENTES
      *                'N' - NAO VIGENTES
      *                'P' - VIGENTES OU POSTERIORES
      *                'H' - HISTORICO
      *                'F' - FUTURAS
