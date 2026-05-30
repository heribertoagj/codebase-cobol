      *----------------------------------------------------------------*
      *            COMMAREA PARA PROGRAMA BVVEWE10                     *
      *            COMUNICACAO COM A CIP - MENSAGEM RRC0010            *
      *----------------------------------------------------------------*
      *================================================================*
      *                        A L T E R A C A O                       *
      *----------------------------------------------------------------*
      *    PROGRAMADOR.:  CLERISTON M SOUZA        - FOURSYS           *
      *    ANALISTA....:  JOSEMAR SIVEK            - DDS               *
      *    DATA........:   15/06/2023                                  *
      *----------------------------------------------------------------*
      *    OBJETIVOS...:   INCLUSAO DO CAMPO NOVO PARA ATENDER A       *
      *                    SOLICITACAO DA CIP DE ENVIO DE MAIS DADOS   *
      *                    PARA PESQUISA DE AGENDA.                    *
      *                    CAMPOS NOVOS: BVVEWE10-INTEROPERABILIDADE   *
      *================================================================*
      *
            05     BVVEWE10-MAPA REDEFINES BVVEWE00-MENSAGEM.
      *            CNPJ (COMPLETO OU BASE) OU CPF USU·RIO FINAL RECEBEDO
              10   BVVEWE10-CNPJOUCPFUSUFINRECBDR           PIC X(014).
      *            CÛDIGO INSTITUIDOR ARRANJO PAGAMENTO
              10   BVVEWE10-CODINSTITDRARRAJPGTO            PIC X(003).
      *            CNPJ CREDENCIADORA/SUBCREDENCIADORA.
              10   BVVEWE10-CNPJCREDDRSUB                   PIC X(014).
      *            DATA PREVISTA LIQUIDAÁ„O.
              10   BVVEWE10-DTPREVTLIQUID                   PIC X(010).
      *            VALOR LIVRE.
              10   BVVEWE10-VLRLIVRE                     PIC 9(012)V99.
      *            DATA INICIAL PREVISTA LIQUIDAÁ„O.
              10   BVVEWE10-DTINIPREVTLIQUID                PIC X(010).
      *            DATA FIM PREVISTA LIQUIDAÁ„O.
              10   BVVEWE10-DTFIMPREVTLIQUID                PIC X(010).
      *            INDICADOR TIPO NEGOCIAÁ„O.
              10   BVVEWE10-INDRTPNEGC                      PIC X(002).
      *            CNPJ (COMPLETO OU BASE) OU CPF DO TITULAR
              10   BVVEWE10-CNPJCPFTITLAR                   PIC X(014).
      *            NAO PREENCHER
              10   BVVEWE10-NUMEROPAGINA-X.
                15 BVVEWE10-NUMEROPAGINA                    PIC 9(003).
      *            NAO PREENCHER
              10   BVVEWE10-TAMANHOPAGINA-X.
                15 BVVEWE10-TAMANHOPAGINA                   PIC 9(005).
      *            INDICADOR DE INTEROPERABILIDADE
TON01         10   BVVEWE10-INTEROPERABILIDADE              PIC X(001).
