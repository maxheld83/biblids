ARG MUGGLE_TAG=756b353ff63d9eafdfc4c870a1508cfd5254f69a
FROM subugoe/muggle-buildtime-onbuild:${MUGGLE_TAG} as buildtime
FROM subugoe/muggle-runtime-onbuild:${MUGGLE_TAG} as runtime
CMD shinycaas::shiny_opts_az(); biblids::doiEntryApp()
