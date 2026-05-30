      *----------------------------------------------------------------*
      *            COMMAREA PARA PROGRAMA BVVEWE19                     *
      *            COMUNICACAO COM A CIP - MENSAGEM RRC0019            *
      *----------------------------------------------------------------*
      *===============================================================*
      *                    A L T E R A C A O                          *
      *===============================================================*
030621*    EMPRESA     : 7COMM                                        *
      *    RESPONSAVEL : TIAGO MACEDO DA SILVA                        *
      *    DATA        : 03/06/2021                                   *
      *    OBJETIVO    : INCLUIR CAMPO CPF/CNPJ DA CREDENCIADORA      *
      *===============================================================*
      *
            05     FILLER        REDEFINES BVVEWE00-MENSAGEM.
      *            TIPO DE COMUNICACAO
      *            1 - INCLUIR UM CONJUNTO DE UNIDADES RECEBÌVEIS
      *            2 - INCLUIR LOTES DE UNIDADES RECEBÌVEIS EM UM
      *                CONJUNTO DE UNIDADES RECEBÌVEIS
      *                UTILIZAR O BVVEWE19-MAPA-2
      *                UTILIZAR O BVVEWE19-MAPA-3
      *            4 - INCLUIR OPERAÁ„O - GESTAO ER
      *                UTILIZAR O BVVEWE19-MAPA-4
      *            5 - ATUALIZAR OPERAÁ„O - GESTAO PARTICIPANTE
      *                UTILIZAR O BVVEWE19-MAPA-5
      *            6 - ATUALIZAR OPERAÁ„O - GESTAO ER
      *                UTILIZAR O BVVEWE19-MAPA-6
             07    BVVEWE19-TP-COMU                         PIC 9(001).
      *            IDENTIFICADOR DO CONJUNTO DE UNIDADES RECEBÌVEIS
      *            PARA INCLUSAO DE LOTES
             07    BVVEWE19-IDENTDCONJUNIRECBVL-P           PIC X(019).
      *            IDENTIFICADOR DA OPERAÁ„O PARA ALTERAÁ„O
             07    BVVEWE19-IDENTD-OP-P                     PIC X(019).
             07    BVVEWE19-MAPA                          PIC X(21830).
             07    BVVEWE19-MAPA-2 REDEFINES BVVEWE19-MAPA.
              10   FILLER OCCURS 50 TIMES.
      *            CNPJ (COMPLETO OU BASE) OU CPF DO TITULAR
               15  BVVEWE19-CNPJCPFTITLAR-2                 PIC X(014).
      *            CNPJ OU CPF TITULAR CONTA
               15  BVVEWE19-CNPJOUCPFTITLARCT-2             PIC X(014).
               15  BVVEWE19-ISPBBCORECBDR-2                 PIC X(008).
      *            TIPO CONTA
               15  BVVEWE19-TPCT-2                          PIC X(002).
      *            AGÍNCIA
               15  BVVEWE19-AG-2                            PIC 9(004).
      *            CONTA
               15  BVVEWE19-CT-2                            PIC 9(013).
*******------------CONTA PAGAMENTO
**************-15  BVVEWE19-CTPGTO-2                        PIC-9(020).
      *            CNPJ CREDENCIADORA/SUBCREDENCIADORA
               15  BVVEWE19-CNPJCREDDRSUB-2                 PIC X(014).
      *            CNPJ OU CPF USU·RIO FINAL RECEBEDOR
               15  BVVEWE19-CNPJCPFUSUFINRECBDR-2           PIC X(014).
      *            CNPJ OU CPF TITULAR
               15  BVVEWE19-CNPJOUCPFTITLAR-2               PIC X(014).
      *            CÛDIGO INSTITUIDOR ARRANJO PAGAMENTO
               15  BVVEWE19-CODINSTITDRARRAJPGT-2           PIC X(003).
      *            DATA PREVISTA LIQUIDAÁ„O
               15  BVVEWE19-DTPREVTLIQUID-2                 PIC X(010).
      *            VALOR OU PERCENTUAL NEGOCIADO
             07    BVVEWE19-MAPA-3 REDEFINES BVVEWE19-MAPA.
      *            TIPO DE GESTAO - "P" PARTICIPANTE "E" ER
              10   BVVEWE19-TPOBJ-3                         PIC X(001).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-IDENTDNEGCRECBVL-3              PIC X(040).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-INDRTPNEGC-3                    PIC X(002).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-DTVENCOP-3                      PIC X(010).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-VLRTOTLIMOUSLDDEVDR-3        PIC 9(012)V99.
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-INDRGESTER-3                    PIC X(001).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-INDRREGRDIVS-3                  PIC X(001).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-INDRACTEINCONDLOP-3             PIC X(001).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-IDENTDCONJUNIRECBVL-3           PIC X(019).
      *            DESCRICAO NAO INFORMADA PELA CIP
      *            INDICADOR ACEITE UNIDADE DE RECEBIVEL RESERVADA
              10   BVVEWE19-INDRACTEUNIDRECBVLR-3           PIC X(001).
             07    BVVEWE19-MAPA-4 REDEFINES BVVEWE19-MAPA.
      *            TIPO DE GESTAO - "P" PARTICIPANTE "E" ER
              10   BVVEWE19-TPOBJ-4                         PIC X(001).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-IDENTDNEGCRECBVL-4              PIC X(040).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-INDRTPNEGC-4                    PIC X(002).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-DTVENCOP-4                      PIC X(010).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-VLRTOTLIMOUSLDDEVDR-4        PIC 9(012)V99.
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-VLRGAR-4                     PIC 9(012)V99.
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-INDRGESTER-4                    PIC X(001).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-INDRREGRDIVS-4                  PIC X(001).
      *            DESCRICAO NAO INFORMADA PELA CIP
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-INDRACTEINCONDLOP-4             PIC X(001).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-INDRACTEUNIRECRES-4             PIC X(001).
      *            DESCRICAO NAO INFORMADA PELA CIP
030621        10   BVVEWE19-TITULARES OCCURS 40 TIMES.
      *            DESCRICAO NAO INFORMADA PELA CIP
               15  BVVEWE19-CNPJCPFTITLAR-4              PIC X(014).
      *            DESCRICAO NAO INFORMADA PELA CIP
               15  BVVEWE19-VLPCUNIDDRECBVL-4            PIC 9(012)V99.
      *            DESCRICAO NAO INFORMADA PELA CIP
               15  BVVEWE19-DTINIOP-4                       PIC X(010).
      *            DESCRICAO NAO INFORMADA PELA CIP
               15  BVVEWE19-DTFIMOP-4                       PIC X(010).
      *            DESCRICAO NAO INFORMADA PELA CIP
               15  BVVEWE19-CNPJOUCPFTITLARCT-4             PIC X(014).
      *            DESCRICAO NAO INFORMADA PELA CIP
               15  BVVEWE19-ISPBBCORECBDR-4                 PIC X(008).
      *            DESCRICAO NAO INFORMADA PELA CIP
               15  BVVEWE19-TPCT-4                          PIC X(002).
               15  BVVEWE19-AG-4                            PIC 9(004).
      *            DESCRICAO NAO INFORMADA PELA CIP
               15  BVVEWE19-CT-4                            PIC 9(013).
0306  *            CREDENCIADORAS
.              15  BVVEWE19-CNPJCREDDRSUB-4                 PIC X(014)
.                                                  OCCURS 15 TIMES.
      *            DESCRICAO NAO INFORMADA PELA CIP
               15  BVVEWE19-CODINSTDRARRAJPGTO-4    PIC X(003)
                                                   OCCURS 10 TIMES.
      *            DESCRICAO NAO INFORMADA PELA CIP
               15  BVVEWE19-CNPJCPFUSUFINRECBDR-4
                                         PIC X(014) OCCURS 10 TIMES.
      *            DESCRICAO CNPJ OU CPF TITULAR CONTA
              10  BVVEWE19-CNPJOUCPFCESAUTD-4    PIC X(014)
                                                   OCCURS 10 TIMES.
      *            IDENTIFICADOR OPERAÁ„O ORIGINAL RENEGOCIAÁ„O DIVIDA
              10  BVVEWE19-IDENTDOPORRENEGCDIV-4 PIC X(019)
                                                   OCCURS 10 TIMES.
             07    BVVEWE19-MAPA-5 REDEFINES BVVEWE19-MAPA.
      *            TIPO DE GESTAO - "P" PARTICIPANTE "E" ER
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-IDENTDNEGCRECBVL-5              PIC X(040).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-INDRTPNEGC-5                    PIC X(002).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-DTVENCOP-5                      PIC X(010).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-VLRTOTLIMOUSLDDEVDR-5        PIC 9(012)V99.
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-INDRGESTER-5                    PIC X(001).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-INDRREGRDIVS-5                  PIC X(001).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-INDRALCANCONTCRESUB-5           PIC X(001).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-INDRACTEINCONDLOP-5             PIC X(001).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-INDRACTEUNIRECRES-5             PIC X(001).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-IDENTDOPDESTCNEGREC-5           PIC X(019).
              10   BVVEWE19-IDENTDCONJUNIRECBVL-5           PIC X(019).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-INDRAUTCCESS-5                  PIC X(001).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-CPNJOUCPFCESAUTD-5     PIC X(014)
                                                   OCCURS 10 TIMES.
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-IDENTDOPORRENEGCDIV-5
                                         PIC X(019) OCCURS 10 TIMES.
             07    BVVEWE19-MAPA-6 REDEFINES BVVEWE19-MAPA.
      *            TIPO DE GESTAO - "P" PARTICIPANTE "E" ER
              10   BVVEWE19-TPOBJ-6                         PIC X(001).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-IDENTDNEGCRECBVL-6              PIC X(040).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-INDRTPNEGC-6                    PIC X(002).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-DTVENCOP-6                      PIC X(010).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-VLRTOTLIMOUSLDDEVDR-6        PIC 9(012)V99.
              10   BVVEWE19-VLRGAR-6                     PIC 9(012)V99.
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-INDRGESTER-6                    PIC X(001).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-INDRREGRDIVS-6                  PIC X(001).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-INDRALCANCONTCRESUB-6    PIC X(001).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-INDRACTEINCONDLOP-6             PIC X(001).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-INDRACTEUNIRECRES-6             PIC X(001).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-IDENTDCONJUNIRECBVL-6           PIC X(019).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-INDRAUTCCESS-6                  PIC X(001).
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-CPNJOUCPFCESAUTD-6     PIC X(014)
                                                   OCCURS 10 TIMES.
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-IDENTDOPORRENEGCDIV-6
      *            DESCRICAO NAO INFORMADA PELA CIP
              10   BVVEWE19-TITULARES-6 OCCURS 6 TIMES.
      *            DESCRICAO NAO INFORMADA PELA CIP
               15  BVVEWE19-CNPJCPFTITLAR-6                 PIC X(014).
      *            DESCRICAO NAO INFORMADA PELA CIP
               15  BVVEWE19-VLPCUNIDDRECBVL-6            PIC 9(012)V99.
      *            DESCRICAO NAO INFORMADA PELA CIP
               15  BVVEWE19-DTINIOP-6                       PIC X(010).
      *            DESCRICAO NAO INFORMADA PELA CIP
               15  BVVEWE19-DTFIMOP-6                       PIC X(010).
      *            DESCRICAO NAO INFORMADA PELA CIP
               15  BVVEWE19-CNPJOUCPFTITLARCT-6             PIC X(014).
      *            DESCRICAO NAO INFORMADA PELA CIP
               15  BVVEWE19-ISPBBCORECBDR-6                 PIC X(008).
      *            DESCRICAO NAO INFORMADA PELA CIP
               15  BVVEWE19-TPCT-6                          PIC X(002).
      *            DESCRICAO NAO INFORMADA PELA CIP
               15  BVVEWE19-AG-6                            PIC 9(004).
      *            CONTA
               15  BVVEWE19-CT-6                            PIC 9(013).
               15  BVVEWE19-CNPJCPFUSUFINRECBDR-6
                                         PIC X(014) OCCURS 10 TIMES.
