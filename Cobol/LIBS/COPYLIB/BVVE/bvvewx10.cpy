      *----------------------------------------------------------------*
      *            COMMAREA PARA PROGRAMA  BVVEWX10                    *
      *            COMUNICACAO COM A CIP - MENSAGEM RRC0010 - TIPO 'U' *
      *----------------------------------------------------------------*
            05 BVVEWX10-DADOS.
               10 BVVEWX10-BLOCO-01.
      * BLOCO DE IDENTIFICACAO DO LAYOUT
                 15 BVVEWX10-IDEN-BLOCO-01.
                    20 FILLER                             PIC X(001).
                    20 BVVWX10-ID-BLOCO-01                PIC 9(002).
                    20 FILLER                             PIC X(001).
      * TIPO DE OBJETO (TITULAR - T, USUARIO FINAL RECEBEDOR - U)
                 15 BVVEWX10-TPOBJ                        PIC X(001).
      * CNPJ ENTIDADE REGISTRADORA
                 15 BVVEWX10-CNPJER                       PIC X(014).
      * CNPJ CREDENCIADORA/SUBCREDENCIADORA
                 15 BVVEWX10-CNPJCREDDRSUB                PIC X(014).
      * CÛDIGO INSTITUIDOR ARRANJO PAGAMENTO
                 15 BVVEWX10-CODINSTITDRARRAJPGT          PIC X(003).
                 15 BVVEWX10-CNPJCPFUSUFINALRECB          PIC X(014).
      * VALOR LIVRE USU·RIO FINAL RECEBEDOR
                 15 BVVEWX10-VLRLIVREUSUFINALREC          PIC 9(012)V99.
      * DATA PREVISTA LIQUIDAÁ„O
                 15 BVVEWX10-DTPREVTLIQUID                PIC X(010).
      * VALOR TOTAL
                 15 BVVEWX10-VLRTOT                       PIC 9(012)V99.
      * INDICADOR DE DOMICILIO
                 15 BVVEWX10-INDRDOMCL                    PIC X(001).
      * TITULARES
      *          15 FILLER OCCURS 01 TIMES.
               10 BVVEWX10-BLOCO-02.
      * BLOCO DE IDENTIFICACAO DO LAYOUT
                 15 BVVEWX10-IDEN-BLOCO-02.
                    20 FILLER                             PIC X(001).
                    20 BVVWX10-ID-BLOCO-02                PIC 9(002).
                    20 FILLER                             PIC X(001).
      * CNPJ OU CPF TITULAR
                 15 BVVEWX10-CNPJOUCPFTITLAR              PIC X(014).
      * VALOR TOTAL TITULAR
      * VALOR COMPROMETIDO EM OUTRAS INSTITUIÁıES
                 15 BVVEWX10-VLRCOMPRTDOUTRINST           PIC 9(012)V99.
      * VALOR COMPROMETIDO NA INSTITUIÁ„O
                 15 BVVEWX10-VLRCOMPRTDINST               PIC 9(012)V99.
      * VALOR LIVRE TOTAL
                 15 BVVEWX10-VLRLIVRETOT                  PIC 9(012)V99.
      * VALOR LIVRE ANTECIPAÁ„O CREDENCIADORA/SUBCREDENCIADORA
                 15 BVVEWX10-VLRLIVREANTECCREDDR          PIC 9(012)V99.
      * VALOR PRÈ-CONTRATADO
                 15 BVVEWX10-VLRPRECONTRD                 PIC 9(012)V99.
      * VALOR ‘NUS RESERVA TÈCNICA
                 15 BVVEWX10-VLRONUSRESTEC                PIC 9(012)V99.
      * DOMICILIOS
      *              20 FILLER OCCURS 10 TIMES.
               10 BVVEWX10-BLOCO-03.
      * BLOCO DE IDENTIFICACAO DO LAYOUT
                 15 BVVEWX10-IDEN-BLOCO-03.
                    20 FILLER                             PIC X(001).
                    20 BVVWX10-ID-BLOCO-03                PIC 9(002).
                    20 FILLER                             PIC X(001).
                 15 BVVEWX10-CNPJOUCPFTITLARCT            PIC X(014).
      * TIPO CONTA
                 15 BVVEWX10-TPCT                         PIC X(002).
      * AGÍNCIA
                 15 BVVEWX10-AG                           PIC 9(005).
      * CONTA
                 15 BVVEWX10-CT                           PIC 9(013).
      * CONTA PAGAMENTO
                 15 BVVEWX10-CTPGTO                       PIC 9(018).
      * DATA EFETIVA LIQUIDAÁ„O
                 15 BVVEWX10-DTEFTLIQUID                  PIC X(010).
      * VALOR EFETIVO LIQUIDAÁ„O
                 15 BVVEWX10-VLREFTLIQUID                 PIC 9(012)V99.
      * VALOR LIVRE
                 15 BVVEWX10-VLRLIVRE                     PIC 9(012)V99.
      * OPERACOES
      *                 25 FILLER OCCURS 15 TIMES.
               10 BVVEWU1-BLOCO-04.
      * BLOCO DE IDENTIFICACAO DO LAYOUT
                 15 BVVEWX10-IDEN-BLOCO-04.
                    20 BVVWX10-ID-BLOCO-04                PIC 9(002).
                    20 FILLER                             PIC X(001).
      * IDENTIFICADOR DA OPERAÁ„O.
                 15 BVVEWX10-IDENTDOP                     PIC X(019).
      * PRIORIDADE NEGOCIAÁ„O RECEBÌVEL
                 15 BVVEWX10-PRIORDDNEGCRECBVL            PIC X(009).
      * CNPJ ENTIDADE REGISTRADORA
                 15 BVVEWX10-CNPJER                       PIC X(014).
      * VALOR NEGOCIADO
                 15 BVVEWX10-VLRNEGCD                     PIC 9(012)V99.
      * VALOR OU PERCENTUAL A CONSTITUIR IF CREDORA
                 15 BVVEWX10-VLRPERCNEGCDCONSTI           PIC 9(012)V99.
      * INDICADOR REGRA DIVIS„O
                 15 BVVEWX10-INDRREGRDIVS                 PIC X(001).
      * DATA FIM DO CONTRATO IF CREDORA
                 15 BVVEWX10-DTFIMOP                      PIC X(010).
      * OPERACOES DE OUTRAS INSTITUICOES
      *           20 FILLER OCCURS 15 TIMES.
               10 BVVEWX10-BLOCO-05.
      * BLOCO DE IDENTIFICACAO DO LAYOUT
                    20 FILLER                             PIC X(001).
                    20 BVVWX10-ID-BLOCO-05                PIC 9(002).
                    20 FILLER                             PIC X(001).
      * INDICADOR REGRA DIVIS„O
                 15 BVVEWX10-INDRREGRDIVS                 PIC X(001).
      * VALOR NEGOCIADO
                 15 BVVEWX10-VLRNEGCD                     PIC 9(012)V99.
      * VALOR OU PERCENTUAL A CONSTITUIR IF CREDORA
                 15 BVVEWX10-VLRPERCNEGCDCONSTI           PIC 9(012)V99.
      * DATA FIM DO CONTRATO IF CREDORA
                 15 BVVEWX10-DTFIMOP                      PIC X(010).
      * PRIORIDADE NEGOCIAÁ„O RECEBÌVEL
                 15 BVVEWX10-PRIORDDNEGCRECBVL            PIC 9(009).
      *
      *----------------------------------------------------------------*
      *            COMMAREA PARA PROGRAMA BVVEWX10                     *
      *            COMUNICACAO COM A CIP - MENSAGEM RRC0010 - TIPO 'T' *
      *----------------------------------------------------------------*
      *
               10 BVVEWX10-BLOCO-06.
                 15 BVVEWX10-IDEN-BLOCO-06.
                    20 FILLER                             PIC X(001).
                    20 BVVWX10-ID-BLOCO-06                PIC 9(002).
                    20 FILLER                             PIC X(001).
      * TIPO DE OBJETO (TITULAR - T, USUARIO FINAL RECEBEDOR - U)
                 15 BVVEWX10-TPOBJ-T                      PIC X(001).
      * CNPJ ENTIDADE REGISTRADORA
                 15 BVVEWX10-CNPJER-T                     PIC X(014).
      * CNPJ CREDENCIADORA/SUBCREDENCIADORA
                 15 BVVEWX10-CNPJCREDDRSUB-T              PIC X(014).
      * CÛDIGO INSTITUIDOR ARRANJO PAGAMENTO
                 15 BVVEWX10-CODINSTITDRARRAJPGT-T        PIC X(003).
      * CNPJ OU CPF TITULAR
                 15 BVVEWX10-CNPJOUCPFTITLAR-T            PIC X(014).
      * DATA PREVISTA LIQUIDAÁ„O
                 15 BVVEWX10-DTPREVTLIQUID-T              PIC X(010).
      * VALOR TOTAL TITULAR
                 15 BVVEWX10-VLRTOTTITLAR-T               PIC 9(012)V99.
      * VALOR COMPROMETIDO EM OUTRAS INSTITUIÁıES
                 15 BVVEWX10-VLRCOMPRTDOUTRINST-T         PIC 9(012)V99.
                 15 BVVEWX10-VLRCOMPRTDINST-T             PIC 9(012)V99.
      * VALOR LIVRE TOTAL
                 15 BVVEWX10-VLRLIVRETOT-T                PIC 9(012)V99.
      * VALOR LIVRE ANTECIPAÁ„O CREDENCIADORA/SUBCREDENCIADORA
                 15 BVVEWX10-VLRLIVREANTECCREDDR-T        PIC 9(012)V99.
      * VALOR PRÈ-CONTRATADO
                 15 BVVEWX10-VLRPRECONTRD-T               PIC 9(012)V99.
      * VALOR ‘NUS RESERVA TÈCNICA
                 15 BVVEWX10-VLRONUSRESTEC-T              PIC 9(012)V99.
      *****************************************************************
