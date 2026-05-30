      *----------------------------------------------------------------*
      *            COMMAREA PARA PROGRAMA BVVEWR28                     *
      *            COMUNICACAO COM A CIP - MENSAGEM RRC0028            *
      *----------------------------------------------------------------*
      *
            05     BVVEWR28-MAPA REDEFINES BVVEWR00-MENSAGEM.
      *            IDENTIFICADOR CONTROLE OPT-IN.
              10   BVVEWR28-IDENTDCTRLOPTIN                 PIC X(019).
      *            CNPJ (COMPLETO OU BASE) OU CPF DO USU·RIO FINAL
      *            RECEBEDOR OU DO TITULAR DO RECEBÌVEL.
              10   BVVEWR28-CNPJCPFUSUFINRECTIT
                                                            PIC X(014).
      *            CNPJ CREDENCIADORA/SUBCREDENCIADORA
              10   BVVEWR28-CNPJCREDDRSUB                   PIC X(014).
      *            CNPJ FINANCIADOR.
              10   BVVEWR28-CNPJFINCDR                      PIC X(014).
      *            CÛDIGO INSTITUIDOR ARRANJO PAGAMENTO.
              10   BVVEWR28-CODINSTITDRARRAJPGTO            PIC X(003).
      *            DATA ASSINATURA OPT-IN.
              10   BVVEWR28-DTOPTIN                         PIC X(010).
      *            DATA INÌCIO OPT-IN.
              10   BVVEWR28-DTINIOPTIN                      PIC X(010).
      *            DATA FIM OPT-IN.
              10   BVVEWR28-DTFIMOPTIN                      PIC X(010).
      *            INDICADOR DE DOMICILIO. INDICADOR CONFORME DOMÌNIO.
              10   BVVEWR28-INDRDOMCL                       PIC X(001).
      *            CNPJ ENTIDADE REGISTRADORA.
              10   BVVEWR28-CNPJER                          PIC X(014).
      *            CPF OU CNPJ RECEBEDOR OPT-IN.
              10   BVVEWR28-CPFOUCNPJRECBDROPTIN            PIC X(014).
