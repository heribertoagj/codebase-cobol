      *----------------------------------------------------------------*
      *            COMMAREA PARA PROGRAMA BVVEWR14                     *
      *            COMUNICACAO COM A CIP - MENSAGEM RRC0014            *
      *----------------------------------------------------------------*
      *
            05     BVVEWR14-MAPA REDEFINES BVVEWR00-MENSAGEM.
      *            CNPJ ENTIDADE REGISTRADORA.
              10   BVVEWR14-CNPJER                          PIC X(014).
      *            IDENTIFICADOR NEGOCIAÁ„O RECEBÌVEL.
              10   BVVEWR14-IDENTDNEGCRECBVL                PIC X(040).
      *            IDENTIFICADOR DA OPERAÁ„O.
              10   BVVEWR14-IDENTDOP                        PIC X(023).
      *            CNPJ (COMPLETO OU BASE) OU CPF DO TITULAR.
              10   BVVEWR14-CNPJCPFTITLAR                   PIC X(014).
      *            IDENTIFICADOR DA OPERAÁ„O DESCONSITUIÁ„O DA
      *            NEGOCIAÁ„O DE RECEBÌVEL
              10   BVVEWR14-IDOPDESCSTCNEGCRECBVL           PIC X(019).
      *            CNPJ CREDENCIADOR/SUBCREDENCIADOR PEDIDO
      *            DESCONTITUIÁ„O NEGOCIAÁ„O RECEBÌVEL.
              10   BVVEWR14-CNPJCREDSUBPEDDESCNEG
                                                            PIC X(014).
              10   FILLER OCCURS 20 TIMES.
      *            CNPJ CREDENCIADORA/SUBCREDENCIADORA
                15 BVVEWR14-CNPJCREDDRSUB                   PIC X(014).
      *            CNPJ OU CPF USU·RIO FINAL RECEBEDOR
                15 BVVEWR14-CNPJCPFUSUFINALRECBDR           PIC X(014).
      *            CNPJ OU CPF TITULAR CONTA.
                15 BVVEWR14-CNPJOUCPFTITLAR                 PIC X(014).
      *            CÛDIGO INSTITUIDOR ARRANJO PAGAMENTO
                15 BVVEWR14-CODINSTITDRARRAJPGTO            PIC X(003).
      *            DATA PREVISTA LIQUIDAÁ„O
                15 BVVEWR14-DTPREVTLIQUID                   PIC X(010).
      *            VALOR DESCONSTITUIR.
                15 BVVEWR14-VLRDESCST-D             PIC 9(009) COMP-4.
                15 BVVEWR14-VLRDESCST-X             PIC X(004).
                15 BVVEWR14-VLRDESCST-9 REDEFINES BVVEWR14-VLRDESCST-X
                                                  USAGE COMP-2.
