      *----------------------------------------------------------------*
      *            COMMAREA PARA PROGRAMA BVVEWU10                     *
      *            COMUNICACAO COM A CIP - MENSAGEM RRC0010 - TIPO 'U' *
      *----------------------------------------------------------------*
      *
            05     BVVEWU10-MAPA REDEFINES BVVEWR00-MENSAGEM.
      * TIPO DE OBJETO (TITULAR - T, USUARIO FINAL RECEBEDOR - U)
              10   BVVEWU10-TPOBJ                           PIC X(001).
      * CNPJ ENTIDADE REGISTRADORA
              10   BVVEWU10-CNPJER                          PIC X(014).
      * CNPJ CREDENCIADORA/SUBCREDENCIADORA
              10   BVVEWU10-CNPJCREDDRSUB                   PIC X(014).
      * CÛDIGO INSTITUIDOR ARRANJO PAGAMENTO
              10   BVVEWU10-CODINSTITDRARRAJPGTO            PIC X(003).
      * CNPJ (COMPLETO OU BASE) OU CPF USU·RIO FINAL RECEBEDOR
              10   BVVEWU10-CNPJCPFUSUFINALRECBDR           PIC X(014).
      * VALOR LIVRE USU·RIO FINAL RECEBEDOR
              10   BVVEWU10-VLRLIVREUSUFINALRECBD        PIC 9(012)V99.
      * DATA PREVISTA LIQUIDAÁ„O
              10   BVVEWU10-DTPREVTLIQUID                   PIC X(010).
      * VALOR TOTAL
              10   BVVEWU10-VLRTOT                       PIC 9(012)V99.
      * INDICADOR DE DOMICILIO
              10   BVVEWU10-INDRDOMCL                       PIC X(001).
      * TITULARES
              10   FILLER OCCURS 01 TIMES.
      * CNPJ OU CPF TITULAR
                15 BVVEWU10-CNPJOUCPFTITLAR                 PIC X(014).
      * VALOR TOTAL TITULAR
                15 BVVEWU10-VLRTOTTITLAR                 PIC 9(012)V99.
      * VALOR COMPROMETIDO EM OUTRAS INSTITUIÁıES
                15 BVVEWU10-VLRCOMPRTDOUTRINST           PIC 9(012)V99.
      * VALOR COMPROMETIDO NA INSTITUIÁ„O
                15 BVVEWU10-VLRCOMPRTDINST               PIC 9(012)V99.
      * VALOR LIVRE TOTAL
                15 BVVEWU10-VLRLIVRETOT                  PIC 9(012)V99.
      * VALOR LIVRE ANTECIPAÁ„O CREDENCIADORA/SUBCREDENCIADORA
                15 BVVEWU10-VLRLIVREANTECCREDDRSU        PIC 9(012)V99.
      * VALOR PRÈ-CONTRATADO
                15 BVVEWU10-VLRPRECONTRD                 PIC 9(012)V99.
      * VALOR ‘NUS RESERVA TÈCNICA
                15 BVVEWU10-VLRONUSRESTEC                PIC 9(012)V99.
      * DOMICILIOS
                15 FILLER OCCURS 10 TIMES.
      * CNPJ OU CPF TITULAR CONTA
                 20 BVVEWU10-CNPJOUCPFTITLARCT               PIC X(014).
      * TIPO CONTA
                 20 BVVEWU10-TPCT                            PIC X(002).
      * AGÍNCIA
                 20 BVVEWU10-AG                              PIC 9(005).
      * CONTA
                 20 BVVEWU10-CT                              PIC 9(013).
      * CONTA PAGAMENTO
                 20 BVVEWU10-CTPGTO                          PIC 9(018).
      * DATA EFETIVA LIQUIDAÁ„O
                 20 BVVEWU10-DTEFTLIQUID                     PIC X(010).
      * VALOR EFETIVO LIQUIDAÁ„O
                 20 BVVEWU10-VLREFTLIQUID                 PIC 9(012)V99.
      * VALOR LIVRE
                 20 BVVEWU10-VLRLIVRE                     PIC 9(012)V99.
      * OPERACOES
                 20 FILLER OCCURS 15 TIMES.
      * IDENTIFICADOR DA OPERAÁ„O.
                  25 BVVEWU10-IDENTDOP                       PIC X(019).
      * PRIORIDADE NEGOCIAÁ„O RECEBÌVEL
                  25 BVVEWU10-PRIORDDNEGCRECBVL              PIC X(009).
      * CNPJ ENTIDADE REGISTRADORA
                  25 BVVEWU10-CNPJER                         PIC X(014).
      * VALOR NEGOCIADO
                  25 BVVEWU10-VLRNEGCD                    PIC 9(012)V99.
      * VALOR OU PERCENTUAL A CONSTITUIR IF CREDORA
                  25 BVVEWU10-VLRPERCNEGCDCONSTITR        PIC 9(012)V99.
      * INDICADOR REGRA DIVIS„O
                  25 BVVEWU10-INDRREGRDIVS                   PIC X(001).
      * DATA FIM DO CONTRATO IF CREDORA
                  25 BVVEWU10-DTFIMOP                        PIC X(010).
      * OPERACOES DE OUTRAS INSTITUICOES
               15  FILLER OCCURS 15 TIMES.
      * INDICADOR REGRA DIVIS„O
                20 BVVEWU10-INDRREGRDIVS                    PIC X(001).
      * VALOR NEGOCIADO
                20 BVVEWU10-VLRNEGCD                     PIC 9(012)V99.
      * VALOR OU PERCENTUAL A CONSTITUIR IF CREDORA
                20 BVVEWU10-VLRPERCNEGCDCONSTITR         PIC 9(012)V99.
      * DATA FIM DO CONTRATO IF CREDORA
                20 BVVEWU10-DTFIMOP                         PIC X(010).
      * PRIORIDADE NEGOCIAÁ„O RECEBÌVEL
                20 BVVEWU10-PRIORDDNEGCRECBVL               PIC 9(009).
