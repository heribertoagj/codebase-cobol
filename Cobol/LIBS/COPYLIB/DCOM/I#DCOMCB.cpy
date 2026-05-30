      ***===========================================================***
      *** NOME INC                                     LENGTH=00919 ***
      *** I#DCOMCB  -  DESCONTO COMERCIAL                           ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: ARQUIVO RETORNO INCONSISTENTES                 ***
      ***                                                           ***
      ***===========================================================***
       01  CBON34-REGISTRO.
           05  CBON34-CIDTFD-PRODT           PIC  9(003).
           05  CBON34-CNEGOC-COBR.
               10  CBON34-DEPDC              PIC  9(004).
               10  CBON34-DEPDIG             PIC  X(001).
               10  CBON34-CTANUM             PIC  9(007).
               10  CBON34-CTADIG             PIC  X(001).
           05  CBON34-CTITLO-COBR.
               10  CBON34-CTITLO-ANO         PIC  9(002).
               10  CBON34-CTITLO-NUM         PIC  9(009).
           05  CBON34-CTITLO-DIG             PIC  X(001).
           05  CBON34-CNRO-SEQ               PIC  9(003).
           05  CBON34-CAUTO-ATDMT            PIC  9(015)  COMP-3.
           05  CBON34-CBAIR-CESP             PIC  9(007).
           05  CBON34-CBCO-DEP               PIC  9(003).
           05  CBON34-CAG-DEP                PIC  9(004).
           05  CBON34-CINDCD-SIT             PIC  9(003)  COMP-3.
           05  CBON34-CINDCD-SACDR           PIC  9(001)  COMP-3.
           05  CBON34-CINDCD-DEBAU           PIC  9(001)  COMP-3.
           05  CBON34-CINDCD-PROTE           PIC  9(001)  COMP-3.
           05  CBON34-CINDCD-REATV           PIC  9(001)  COMP-3.
           05  CBON34-CINDCD-REAP            PIC  9(001)  COMP-3.
           05  CBON34-CINDCD-PGTOV           PIC  9(001)  COMP-3.
           05  CBON34-CINDCD-RTEIO           PIC  X(001).
           05  CBON34-CALT-DATAV             PIC  9(001)  COMP-3.
           05  CBON34-QPER-MANUT             PIC  9(001)  COMP-3.
           05  CBON34-DVCTO-AMD              PIC  X(008).
           05  CBON34-DVCTO-AMD-R            REDEFINES
               CBON34-DVCTO-AMD              PIC  9(008).
           05  CBON34-DEMIS-AMD              PIC  X(008).
           05  CBON34-DEMIS-AMD-R            REDEFINES
               CBON34-DEMIS-AMD              PIC  9(008).
           05  CBON34-DREATV                 PIC  9(009)  COMP-3.
           05  CBON34-DREG                   PIC  9(009)  COMP-3.
           05  CBON34-CIDTFD                 PIC  9(005).
           05  CBON34-QMOEDA-VLR-X.
               10  CBON34-QMOEDA-VLR         PIC  9(015).
           05  CBON34-QMOEDA-ABTMT-X.
               10  CBON34-QMOEDA-ABTMT       PIC  9(015).
           05  CBON34-QMOEDA-IOF             PIC  9(015).
           05  CBON34-CTITLO-CLI             PIC  X(015).
           05  CBON34-RCTRL-CDENT            PIC  X(025).
           05  CBON34-ISACDO                 PIC  X(040).
           05  CBON34-ELOGDR-COPLT           PIC  X(040).
           05  CBON34-CCEP-X.
               10  CBON34-CCEP               PIC  9(005).
           05  CBON34-CCOMPL-CEP             PIC  X(003).
           05  CBON34-TDOCTO-SACDO-X.
               10  CBON34-TDOCTO-SACDO       PIC  9(001).
           05  CBON34-CDOCTO-SACDO-X.
               10  CBON34-CDOCTO-SACDO       PIC  9(015).
           05  CBON34-STATUS-CGC             PIC  9(001)  COMP-3.
           05  CBON34-CDESC-PRIM-X.
               10  CBON34-CDESC-PRIM         PIC  9(001).
           05  CBON34-DVALDD-PRIM            PIC  X(008).
           05  CBON34-DVALDD-PRIM-R          REDEFINES
               CBON34-DVALDD-PRIM            PIC  9(008).
           05  CBON34-QDESC-PRIM-X.
               10  CBON34-QDESC-PRIM         PIC  9(015).
           05  CBON34-CDESC-SEGDA-X.
               10  CBON34-CDESC-SEGDA        PIC  9(001).
           05  CBON34-DVALDD-SEGDA           PIC  X(008).
           05  CBON34-DVALDD-SEGDA-R         REDEFINES
               CBON34-DVALDD-SEGDA           PIC  9(008).
           05  CBON34-QDESC-SEGDA-X.
               10  CBON34-QDESC-SEGDA        PIC  9(015).
           05  CBON34-CDESC-TERC-X.
               10  CBON34-CDESC-TERC         PIC  9(001).
           05  CBON34-DVALDD-TERC            PIC  X(008).
           05  CBON34-DVALDD-TERC-R          REDEFINES
               CBON34-DVALDD-TERC            PIC  9(008).
           05  CBON34-QDESC-TERC-X.
               10  CBON34-QDESC-TERC         PIC  9(015).
           05  CBON34-CCOMIS-PERMC           PIC  9(001).
           05  CBON34-DCOMIS-PERMC           PIC  X(008).
           05  CBON34-DCOMIS-PERMC-R         REDEFINES
               CBON34-DCOMIS-PERMC           PIC  9(008).
           05  CBON34-QMOEDA-COMIS           PIC  9(015).
           05  CBON34-CMULTA                 PIC  9(001).
           05  CBON34-DMULTA-AMD             PIC  X(008).
           05  CBON34-DMULTA-AMD-R           REDEFINES
               CBON34-DMULTA-AMD             PIC  9(008).
           05  CBON34-QMOEDA-MULTA           PIC  9(015).
           05  CBON34-CALT-PROTE-X.
               10  CBON34-CALT-PROTE         PIC  9(001).
           05  CBON34-QDIA-PROTE-X.
               10  CBON34-QDIA-PROTE         PIC  9(002).
           05  FILLER                        PIC  X(003).
           05  CBON34-CTPO-CART              PIC  9(001).
           05  CBON34-CTPO-DOCTO             PIC  9(001).
           05  CBON34-CEMIS-BLOQT            PIC  9(001).
           05  CBON34-CDISTB-BLOQT           PIC  9(001).
           05  CBON34-CESPCE                 PIC  X(002).
           05  CBON34-CACEIT                 PIC  X(001).
           05  CBON34-DEPDC-SCTRO            PIC  9(005).
           05  CBON34-CTRATO-PPLTA           PIC  9(005).
           05  CBON34-CORIGE-REG             PIC  9(003)  COMP-3.
           05  CBON34-DEPDC-OPER             PIC  9(005).
           05  CBON34-CEMPR-TROCAF           PIC  9(007)  COMP-3.
           05  CBON34-ISACDR-AVALS           PIC  X(040).
           05  CBON34-CTPO-INSCR             PIC  9(001).
           05  CBON34-INSCR-NUM              PIC  X(015).
           05  CBON34-STATUS-SACAV           PIC  9(001)  COMP-3.
           05  CBON34-CBCO-DBAU              PIC  9(003).
           05  CBON34-DEPDC-DBAU             PIC  9(005).
           05  CBON34-DGDEP-DBAU             PIC  9(001).
           05  CBON34-CGRP-CTABIL            PIC  9(003).
           05  CBON34-CSGRP-CTBIL            PIC  9(003).
           05  CBON34-CCTA-CLI               PIC  9(012).
           05  CBON34-CDIG-BDSCO             PIC  9(001).
           05  CBON34-CDIG-EXTER             PIC  9(001).
           05  CBON34-CENDER-AVISO           PIC  9(001).
           05  CBON34-CINDCD-EMIS            PIC  9(001).
           05  CBON34-CTPO-FORMT-DB          PIC  9(003)  COMP-3.
           05  CBON34-CTPO-MOTVO-DB          PIC  9(003)  COMP-3.
           05  CBON34-CMOTVO-TITLO-DB        PIC  9(003)  COMP-3.
           05  CBON34-DDSAGN-DBAU            PIC  9(009)  COMP-3.
           05  CBON34-DINSTR-PROTE           PIC  9(009)  COMP-3.
           05  CBON34-DENVIO-CARTR           PIC  9(009)  COMP-3.
           05  CBON34-DSOLTC-SUSTC           PIC  9(009)  COMP-3.
           05  CBON34-DEFTVC-SUSTC           PIC  9(009)  COMP-3.
           05  CBON34-NUM-CARTORIO           PIC  X(005).
           05  CBON34-NUM-PROTOCOL           PIC  X(015).
           05  CBON34-DEV-SUST               PIC  9(003)  COMP-3.
           05  CBON34-SEQ-EVNTO              PIC  9(013)  COMP-3.
           05  CBON34-COD-USUARIO            PIC  X(007).
           05  CBON34-NUM-LOTE               PIC  9(005)  COMP-3.
           05  CBON34-DMOVIM-AMD             PIC  9(009)  COMP-3.
           05  CBON34-COCOR-REMSS            PIC  9(003)  COMP-3.
           05  CBON34-CMOTVO-REMSS           PIC  9(003)  COMP-3.
           05  CBON34-CORIGE-MOV             PIC  9(003)  COMP-3.
           05  CBON34-DEPDC-PROC             PIC  9(005)  COMP-3.
           05  CBON34-CTPO-OCOR              PIC  9(003)  COMP-3.
           05  CBON34-CTPO-FORMT-MVT         PIC  9(003)  COMP-3.
           05  CBON34-CTPO-MOTVO-MVT         PIC  9(003)  COMP-3.
           05  CBON34-CMOTVO-TITLO-MVT       PIC  9(003)  COMP-3.
           05  CBON34-COCOR-RETOR            PIC  9(003)  COMP-3.
           05  CBON34-CMOTVO-RET1            PIC  9(003)  COMP-3.
           05  CBON34-CMOTVO-RET2            PIC  9(003)  COMP-3.
           05  CBON34-CMOTVO-RET3            PIC  9(003)  COMP-3.
           05  CBON34-CMOTVO-RET4            PIC  9(003)  COMP-3.
           05  CBON34-CMOTVO-RET5            PIC  9(003)  COMP-3.
           05  CBON34-VLR-MOEDA              PIC  9(015)  COMP-3.
           05  CBON34-SGLA-MOEDA             PIC  X(004).
           05  CBON34-DCRED                  PIC  9(009)  COMP-3.
           05  CBON34-DPGTO                  PIC  9(009)  COMP-3.
           05  CBON34-VCOBRD                 PIC  9(015)  COMP-3.
           05  CBON34-VIOF-RTIDO             PIC  9(015)  COMP-3.
           05  CBON34-VTITLO-REAL            PIC  9(015)  COMP-3.
           05  CBON34-VDESC-CONCD            PIC  9(015)  COMP-3.
           05  CBON34-VABTMT-CONCD           PIC  9(015)  COMP-3.
           05  CBON34-VMULTA-PG              PIC  9(015)  COMP-3.
           05  CBON34-VCOMIS-PG              PIC  9(015)  COMP-3.
           05  CBON34-CBCO-COBR              PIC  9(003)  COMP-3.
           05  CBON34-DEPDC-COBR             PIC  9(005)  COMP-3.
           05  CBON34-CEMPRE-ASSOC           PIC  9(007)  COMP-3.
           05  CBON34-CMEIO-TRANSM           PIC  9(003)  COMP-3.
           05  CBON34-CFORMT-REMES           PIC  9(003)  COMP-3.
           05  CBON34-NOPER-CRED             PIC  9(010).
           05  CBON34-BCO-CORRESP            PIC  9(003).
           05  CBON34-NN-BCO-CORRESP         PIC  X(020).
           05  CBON34-VL-TARIFA-REG          PIC  9(7)V99 COMP-3.
           05  CBON34-RES-PASSAGEM           PIC  X(062).
           05  CBON34-TIPO-REG               PIC  X(001).
           05  CBON34-VLR-PARCELA            PIC  9(015)  COMP-3.
           05  CBON34-OUTR-RENDIM            PIC  9(015)  COMP-3.
           05  CBON34-OUTR-DESPESAS          PIC  9(015)  COMP-3.
           05  CBON34-TIPO-LIQUID            PIC  X(002).
           05  CBON34-CD-LOC-EMISS           PIC  9(004).
           05  CBON34-AG-COM-BAIXA           PIC  9(005).
           05  CBON34-SEQUENCIA              PIC  9(005)  COMP-3.
           05  CBON34-SEQUEN-FINAL           PIC  9(009)  COMP-3.
           05  CBON34-ORIG-REJ               PIC  X(001).
           05  CBON34-CINDCD-DECUR-X.
               10  CBON34-CINDCD-DECUR       PIC  9(001).
           05  CBON34-QDIA-DECUR-X.
               10  CBON34-QDIA-DECUR         PIC  9(003).
           05  CBON34-BRUT-COBR              PIC  9(015)  COMP-3.
           05  CBON34-TIPO-ENDOSSO           PIC  X(001).
           05  CBON34-FORMA-PGTO-TITLO       PIC  9(003)  COMP-3.
           05  CBON34-BANCO-CHEQ-PGTO        PIC  9(003)  COMP-3.
           05  CBON34-CMIDIA-PGTO-TITLO      PIC  9(005)  COMP-3.
      *---     HORARIO DE ENVIO DO ARQUIVO RETORNO ---
      *---     HORARIOS VALIDOS:
      *---     DE 09 A 21
           05  CBON34-RET-PARCIAL            PIC  9(002)  COMP-3.
           05  FILLER                        PIC  X(011).
      *---     DADOS ADICIONAIS ---
           05  CBON34-DCOM-ANO-OPER          PIC  9(004).
           05  CBON34-DCOM-SEQ-OPER          PIC  9(009).
           05  CBON34-DCOM-COD-OCORR         PIC  9(002).
           05  CBON34-DCOM-NUM-AUTORIZ       PIC  9(004).
