      ******************************************************************
      * NOME DA INC - I#GFCTWF                                         *
      * DESCRICAO   - CANC. LIBER. SUSPENSAO COBRANCA TARIFAS AGRUP.   *
      *               COMUNICACAO IMS X WEB        -       SAIDA       *
      * TAMANHO     - 1010                                             *
      * FIXO        - 365                                              *
      * OCORRENCIA  - 129                                              *
      * VARIAVEL    - 645                                              *
      * DATA        - 03/2007                                          *
      * RESPONSAVEL - FRANCISCO LOURENCO - PROCWORK - TEL 4152-4352    *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTWF-SAIDA.
           03  GFCTWF-LL                            PIC S9(004) COMP.
           03  GFCTWF-ZZ                            PIC S9(004) COMP.
           03  GFCTWF-TRANSACAO                     PIC  X(009).
           03  GFCTWF-FUNCAO                        PIC  X(008).
           03  GFCTWF-FILTRO.
               05 GFCTWF-FL-COD-AGRUPAMENTO         PIC  9(003).
               05 GFCTWF-FL-COD-TARIFA              PIC  9(005).
           03  GFCTWF-QTDE-OCOR                     PIC  9(003).
           03  GFCTWF-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTWF-FUNC-BDSCO                    PIC  X(007).
           03  GFCTWF-ERRO                          PIC  9(001).
           03  GFCTWF-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTWF-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTWF-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTWF-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(229).
           03  GFCTWF-DADOS.
               05  FILLER                           OCCURS 05.
                   07  GFCTWF-SEQ-AGRUPAMENTO       PIC  9(009).
                   07  GFCTWF-COD-TARIFA            PIC  9(005).
                   07  GFCTWF-DESC-TARIFA           PIC  X(020).
                   07  GFCTWF-COD-AGENCIA           PIC  9(005).
                   07  GFCTWF-COD-POSTO             PIC  9(005).
ST2507*            07  GFCTWF-COD-CNPJ-CPF          PIC  9(016).
ST2507             07  GFCTWF-COD-CNPJ-CPF          PIC  X(015).
                   07  GFCTWF-COD-MUNICIPIO         PIC  9(007).
                   07  GFCTWF-COD-UF                PIC  X(002).
                   07  GFCTWF-DESC-COMPONENTE       PIC  X(020).
                   07  GFCTWF-DATA-INIC-SUSP        PIC  X(010).
                   07  GFCTWF-DATA-FIM-SUSP         PIC  X(010).
                   07  GFCTWF-DATA-COBRANCA         PIC  X(010).
                   07  GFCTWF-COD-MSG-ERRO-GEN      PIC  9(007).
                   07  GFCTWF-COD-SQL-ERRO-GEN      PIC  9(003).
ST2507     03  FILLER                               PIC  X(005).
