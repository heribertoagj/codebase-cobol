      ******************************************************************
      * NOME DA INC - I#GFCTWE                                         *
      * DESCRICAO   - CANC. LIBER. SUSPENSAO COBRANCA TARIFAS AGRUP.   *
      *               COMUNICACAO IMS X WEB        -       ENTRADA     *
      * TAMANHO     - 700                                              *
      * FIXO        - 105                                              *
      * OCORRENCIA  - 119                                              *
      * VARIAVEL    - 595                                              *
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
       01  GFCTWE-ENTRADA.
           03  GFCTWE-LL                            PIC S9(004) COMP.
           03  GFCTWE-ZZ                            PIC S9(004) COMP.
           03  GFCTWE-TRANSACAO                     PIC  X(009).
           03  GFCTWE-FUNCAO                        PIC  X(008).
           03  GFCTWE-FILTRO.
               05 GFCTWE-FL-COD-AGRUPAMENTO         PIC  9(003).
               05 GFCTWE-FL-COD-TARIFA              PIC  9(005).
           03  GFCTWE-QTDE-OCOR                     PIC  9(003).
           03  GFCTWE-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTWE-FUNC-BDSCO                    PIC  X(007).
           03  GFCTWE-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(059).
           03  GFCTWE-DADOS.
               05  FILLER                           OCCURS 05.
                   07  GFCTWE-SEQ-AGRUPAMENTO       PIC  9(009).
                   07  GFCTWE-COD-TARIFA            PIC  9(005).
                   07  GFCTWE-DESC-TARIFA           PIC  X(020).
                   07  GFCTWE-COD-AGENCIA           PIC  9(005).
                   07  GFCTWE-COD-POSTO             PIC  9(005).
ST2507*            07  GFCTWE-COD-CNPJ-CPF          PIC  9(016).
ST2507             07  GFCTWE-COD-CNPJ-CPF          PIC  X(015).
                   07  GFCTWE-COD-MUNICIPIO         PIC  9(007).
                   07  GFCTWE-COD-UF                PIC  X(002).
                   07  GFCTWE-DESC-COMPONENTE       PIC  X(020).
                   07  GFCTWE-DATA-INIC-SUSP        PIC  X(010).
                   07  GFCTWE-DATA-FIM-SUSP         PIC  X(010).
                   07  GFCTWE-DATA-COBRANCA         PIC  X(010).
               05  FILLER                           PIC  X(005).
