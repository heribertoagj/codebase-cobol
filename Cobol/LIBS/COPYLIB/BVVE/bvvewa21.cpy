      *----------------------------------------------------------------*
      *            COMMAREA PARA PROGRAMA BVVEWA21                     *
      *            COMUNICACAO COM A CIP - MENSAGEM RRC0021            *
      *----------------------------------------------------------------*
      *
            05     BVVEWA21-MAPA REDEFINES BVVEWR00-MENSAGEM.
             07    FILLER OCCURS 20 TIMES.
      *            CNPJ (COMPLETO OU BASE) OU CPF DO TITULAR
              10   BVVEWA21-CNPJCPFTITLAR                   PIC X(014).
      *            CNPJ OU CPF TITULAR CONTA
              10   BVVEWA21-CNPJOUCPFTITLARCT               PIC X(014).
      *            ISPB BANCO RECEBEDOR
              10   BVVEWA21-ISPBBCORECBDR                   PIC X(008).
      *            TIPO CONTA
              10   BVVEWA21-TPCT                            PIC X(002).
      *            AGÍNCIA
              10   BVVEWA21-AG                              PIC 9(004).
      *            CONTA
              10   BVVEWA21-CT                              PIC 9(013).
              10   BVVEWA21-CTPGTO                          PIC 9(018).
      *            PRIORIDADE NEGOCIAÁ„O RECEBÌVEL
              10   BVVEWA21-PRIORDDNEGCRECBVL               PIC 9(009).
      *            CNPJ CREDENCIADORA/SUBCREDENCIADORA
              10   BVVEWA21-CNPJCREDDRSUB                   PIC X(014).
      *            CNPJ OU CPF USU·RIO FINAL RECEBEDOR
              10   BVVEWA21-CNPJCPFUSUFINRECBDR             PIC X(014).
      *            CÛDIGO INSTITUIDOR ARRANJO PAGAMENTO
              10   BVVEWA21-CODINSTITDRARRAJPGTO            PIC X(003).
      *            DATA PREVISTA LIQUIDAÁ„O
              10   BVVEWA21-DTPREVTLIQUID                   PIC X(010).
      *            VALOR NEGOCIADO
              10   BVVEWA21-VLRNEGCD-D    PIC 9(009) COMP-4.
              10   BVVEWA21-VLRNEGCD-X    PIC X(004).
              10   BVVEWA21-VLRNEGCD-9  REDEFINES
                   BVVEWA21-VLRNEGCD-X  USAGE COMP-2.
