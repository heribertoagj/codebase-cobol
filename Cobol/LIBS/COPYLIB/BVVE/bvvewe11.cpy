      *----------------------------------------------------------------*
      *            COMMAREA PARA PROGRAMA BVVEWE11                     *
      *            COMUNICACAO COM A CIP - MENSAGEM RRC0011            *
      *----------------------------------------------------------------*
      *
            05     BVVEWE11-MAPA REDEFINES BVVEWE00-MENSAGEM.
      *            IDENTIFICADOR CONTROLE REQUISIÁ„O SOLICITANTE
              10   BVVEWE11-IDENTDCTRLREQSOLICTE            PIC X(020).
      *            CNPJ (COMPLETO OU BASE) OU CPF DO USU·RIO FINAL
      *            RECEBEDOR OU DO TITULAR DO RECEBÌVEL
              10   BVVEWE11-CNPJCPFUSUFINRECTIT             PIC X(014).
      *            CNPJ CREDENCIADORA/SUBCREDENCIADORA
              10   BVVEWE11-CNPJCREDDRSUB                   PIC X(014).
      *            CNPJ OU CPF FINANCIADOR
              10   BVVEWE11-CNPJFINCDR                      PIC X(014).
      *            CÛDIGO INSTITUIDOR ARRANJO PAGAMENTO
              10   BVVEWE11-CODINSTITDRARRAJPGTO            PIC X(003).
      *            DATA ASSINATURA OPT-IN
              10   BVVEWE11-DTOPTIN                         PIC X(010).
              10   BVVEWE11-DTINIOPTIN                      PIC X(010).
      *            DATA FIM OPT-IN
              10   BVVEWE11-DTFIMOPTIN                      PIC X(010).
      *            INDICADOR DE DOMICILIO
              10   BVVEWE11-INDRDOMCL                       PIC X(001).
