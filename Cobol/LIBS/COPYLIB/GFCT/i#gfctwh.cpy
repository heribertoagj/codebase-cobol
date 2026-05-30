      ******************************************************************
      * NOME DA INC - I#GFCTWH                                         *
      * DESCRICAO   - LISTA LIBER. SUSPENSAO COBRANCA TARIFAS AGRUP.   *
      *               COMUNICACAO IMS X WEB - SAIDA                    *
      * TAMANHO     - 1010                                             *
      * FIXO        - 170                                              *
      * OCORRENCIA  - 120                                              *
      * VARIAVEL    - 840                                              *
      * DATA        - 03/2007                                          *
      * RESPONSAVEL - FRANCISCO LOURENCO - PROCWORK - TEL 4152-4352    *
      ******************************************************************
      *
       01  GFCTWH-SAIDA.
           03  GFCTWH-LL                            PIC S9(004) COMP.
           03  GFCTWH-ZZ                            PIC S9(004) COMP.
           03  GFCTWH-TRANSACAO                     PIC  X(009).
           03  GFCTWH-FUNCAO                        PIC  X(008).
           03  GFCTWH-FILTRO.
               05  GFCTWH-FL-IND-LIBERACAO          PIC  X(001).
      *---         'A' - AGENDADA
      *---         'E' - EFETIVADA
      *---         'T' - TODAS
               05  GFCTWH-FL-COD-AGRUPAMENTO        PIC  9(003).
               05  GFCTWH-FL-COD-TARIFA             PIC  9(005).
           03  GFCTWH-QTDE-OCOR                     PIC  9(003).
           03  GFCTWH-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTWH-FUNC-BDSCO                    PIC  X(007).
           03  GFCTWH-ERRO                          PIC  9(001).
           03  GFCTWH-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTWH-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTWH-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTWH-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(033).
           03  GFCTWH-DADOS.
               05  FILLER                           OCCURS 07.
                   07  GFCTWH-SEQ-AGRUPAMENTO       PIC  9(009).
                   07  GFCTWH-COD-TARIFA            PIC  9(005).
                   07  GFCTWH-DESC-TARIFA           PIC  X(020).
                   07  GFCTWH-COD-AGENCIA           PIC  9(005).
                   07  GFCTWH-COD-POSTO             PIC  9(005).
                   07  GFCTWH-COD-CNPJ-CPF          PIC  9(009).
                   07  GFCTWH-NUM-FILIAL            PIC  9(005).
                   07  GFCTWH-COD-CONTROLE          PIC  9(002).
                   07  GFCTWH-COD-MUNICIPIO         PIC  9(007).
                   07  GFCTWH-COD-UF                PIC  X(002).
                   07  GFCTWH-DESC-COMPONENTE       PIC  X(020).
                   07  GFCTWH-DATA-INIC-SUSP        PIC  X(010).
                   07  GFCTWH-DATA-FIM-SUSP         PIC  X(010).
                   07  GFCTWH-DATA-COBRANCA         PIC  X(010).
                   07  GFCTWH-COD-SITUACAO          PIC  X(001).
      *---             'N' - PENDENTE
      *---             'A' - AGENDADA
      *---             'C' - AGENDAMENTO CANCELADO (PENDENTE)
      *---             'E' - EFETIVADA
